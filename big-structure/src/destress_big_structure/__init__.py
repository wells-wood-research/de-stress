import json
import typing as t
import time

from flask import Flask
from flask_cors import CORS
from flask_graphql import GraphQLView
from flask_sockets import Sockets

from .analysis import create_metrics_from_pdb
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

app = Flask(__name__)
sockets = Sockets(app)
CORS(app)
app.debug = True


class ServerJobManager:
    def __init__(
        self,
        server_job: ServerJob,
        websocket,
        out_msg_constructor: t.Callable[[ServerJobStatus], ClientWebsocketIncoming],
    ):
        self.server_job = server_job
        self.websocket = websocket
        self.out_msg_constructor = out_msg_constructor

    @property
    def status(self):
        return self.server_job.status

    @status.setter
    def status(self, new_status):
        self.server_job.status = new_status
        outgoing_message = self.out_msg_constructor(self.server_job)
        self.websocket.send(outgoing_message.to_json())


@sockets.route("/app-comms")
def app_comms_socket(ws):
    """Provides 2-way communication for the app.

    All data will flow through this websocket, to enable live updating
    of the results. All data is sent as JSON and on the Python side should
    be mapped to Python dataclasses.
    """
    while not ws.closed:
        message_string: t.Optional[str] = ws.receive()

        # First check is message is None, seems to do this on init
        if message_string:

            message_dict = json.loads(message_string)
            message = ClientWebsocketOutgoing.from_dict(message_dict)

            if message_dict["tag"] == "RequestMetrics":
                server_job = ServerJobManager(
                    ServerJob.from_dict(
                        message_dict["args"][0], RequestMetricsInput, DesignMetrics
                    ),
                    ws,
                    ClientWebsocketIncoming.RECEIVEDMETRICSJOB,
                )
                # This should always succeed as the job is always the submitted type
                input_pdb_string = server_job.status.submitted().pdb_string

                server_job.status = ServerJobStatus.QUEUED()
                time.sleep(3)

                server_job.status = ServerJobStatus.INPROGRESS()
                time.sleep(3)

                design_metrics = create_metrics_from_pdb(input_pdb_string)
                server_job.status = ServerJobStatus.COMPLETE(design_metrics)
        else:
            print("Received empty message.")


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
