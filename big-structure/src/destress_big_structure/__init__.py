import itertools
import json
import typing as t
import time

from flask import Flask
from flask_cors import CORS
from flask_graphql import GraphQLView
from flask_sockets import Sockets
import gevent
import redis
import rq
from rq.job import Job

from .analysis import create_metrics_from_pdb, JpredSubmission
from .big_structure_models import big_structure_db_session
from .design_models import designs_db_session
from .elm_types import (
    ClientWebsocketIncoming,
    ClientWebsocketOutgoing,
    DesignMetrics,
    RequestMetricsInput,
    ServerJob,
    ServerJobStatus,
)

from .schema import schema

# Flask Setup
app = Flask(__name__)
sockets = Sockets(app)
CORS(app)
app.debug = False

# Job queue setup, the worker runs in the `rq_worker` container
REDIS_CONNECTION = redis.Redis("destress-redis", 6379)
JOB_QUEUE = rq.Queue(connection=REDIS_CONNECTION, default_timeout=30)

# {{{ ServerJobManager


class ServerJobManager:
    """A wrapper that holds all information for a server job including shared state.

    Parameters
    ----------
    server_job: ServerJob
        Contains information required to run the job, this state is shared
        between the client and the server and the `ServerJobManager` attempts
        to sync any changes to `server_job`

    websocket
        Websocket used to communicate with the client.

    out_msg_constructor: Callable[[ServerJob], ClientWebsocketIncoming]
        A function that can create an outgoing message to the client from a
        server job.
    """

    def __init__(
        self,
        server_job,
        websocket,
        out_msg_constructor: t.Callable[[ServerJobStatus], ClientWebsocketIncoming],
    ):
        self.server_job = server_job
        self.websocket = websocket
        self.out_msg_constructor = out_msg_constructor
        self._rq_job_handle: t.Optional[Job] = None
        self._rq_job_last_status: t.Optional[str] = None

    @property
    def status(self):
        """Retrieves `self.server_job.status`."""
        return self.server_job.status

    @status.setter
    def status(self, new_status: ServerJobStatus):
        """Updates `self.server_job.status` and attempts to sync state with client."""
        self.server_job.status = new_status
        if (new_status == ServerJobStatus.CANCELLED()) and (self.rq_job_handle):  # type: ignore
            print(f"Cancelling job {self.server_job.uuid}...")
            self.rq_job_handle.cancel()
        outgoing_message = self.out_msg_constructor(self.server_job)
        self.websocket.send(outgoing_message.to_json())

    @property
    def rq_job_handle(self):
        """A RQ job handle for a queued job.

        Note
        ----
        Once set, the status of the queued job will be used to update the
        status of the `ServerJobManager`.
        """
        return self._rq_job_handle

    @rq_job_handle.setter
    def rq_job_handle(self, job_handle: Job):
        self._rq_job_handle = job_handle
        self.update_job_status()
        return

    def update_job_status(self):
        """Checks the status of a queued job and updates the `self.status`.

        Note
        ----
        If `self._rq_job_handle` is not set, the status will not be updated.
        """
        current_status = self._rq_job_handle.get_status()
        if current_status is None:
            return
        if self._rq_job_last_status != current_status:
            if current_status == "queued":
                self.status = ServerJobStatus.QUEUED()
            elif current_status == "deferred":
                self.status = ServerJobStatus.QUEUED()
            elif current_status == "started":
                self.status = ServerJobStatus.INPROGRESS()
            elif current_status == "finished":
                self.status = ServerJobStatus.COMPLETE(self._rq_job_handle.result)
            elif current_status == "failed":
                self.status = ServerJobStatus.FAILED(
                    f"The job failed to run:\n\n{self._rq_job_handle.exc_info}"
                    if self._rq_job_handle.exc_info
                    else f"The job failed to run:\n\nThe job timed out."
                )
            else:
                self.status = ServerJobStatus.FAILED("Unknown RQ job state.")
            self._rq_job_last_status = current_status
        return


# }}}


@sockets.route("/app-comms")
def app_comms_socket(ws):
    """Provides 2-way communication for the app.

    All data will flow through this websocket, to enable live updating
    of the results. All data is sent as JSON and on the Python side should
    be mapped to Python dataclasses.
    """
    server_jobs: t.Dict[str, ServerJobManager] = {"RequestMetrics": []}
    print("Connected to client.")
    while not ws.closed:
        # Check the websocket for new jobs
        message_string = None
        with gevent.Timeout(2, False):
            message_string: t.Optional[str] = ws.receive()

        # First check is message is None, seems to do this on init
        if message_string:
            (job_type, server_job) = process_client_message(message_string, ws)
            server_jobs[job_type].append(server_job)

        # Update the status of existing jobs
        for s_job in (
            x
            for x in itertools.chain(*list(server_jobs.values()))
            if x.status.match(
                ready=lambda: True,
                submitted=lambda: True,
                queued=lambda: True,
                inprogress=lambda: True,
                cancelled=lambda: False,
                failed=lambda _: False,
                complete=lambda _: False,
            )
        ):
            print(f"Updating job statuses {s_job.server_job}...")
            s_job.update_job_status()


def process_client_message(
    message_string: str, websocket
) -> t.Tuple[str, ServerJobManager]:
    """Creates a server job from an incoming websocket message string."""
    message_dict = json.loads(message_string)
    message = ClientWebsocketOutgoing.from_dict(message_dict)
    if message_dict["tag"] == "RequestMetrics":
        server_job_manager = ServerJobManager(
            server_job=ServerJob.from_dict(
                message_dict["args"][0], RequestMetricsInput, DesignMetrics
            ),
            websocket=websocket,
            out_msg_constructor=ClientWebsocketIncoming.RECEIVEDMETRICSJOB,  # type: ignore
        )
        # This should always succeed as the job is always the submitted type
        input_pdb_string = server_job_manager.status.submitted().pdb_string
        rq_job = Job.create(
            create_metrics_from_pdb, [input_pdb_string], connection=REDIS_CONNECTION,
        )
        rq_job_handle = JOB_QUEUE.enqueue_job(rq_job)
        server_job_manager.rq_job_handle = rq_job_handle
        return message_dict["tag"], server_job_manager
    else:
        raise ValueError(f'Unexpected job type: {message_dict["tag"]}')


app.add_url_rule(
    "/graphql",
    view_func=GraphQLView.as_view(
        "graphql", schema=schema, graphiql=True  # for having the GraphiQL interface
    ),
)


@app.teardown_appcontext
def shutdown_session(exception=None):
    big_structure_db_session.remove()
    designs_db_session.remove()


__version__ = "0.1.0"
