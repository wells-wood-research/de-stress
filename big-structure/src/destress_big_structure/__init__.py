import json
import typing as t

from flask import Flask
from flask_cors import CORS
from flask_graphql import GraphQLView
from flask_sockets import Sockets

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
            print(message.__repr__())

            if message_dict["tag"] == "RequestMetrics":
                server_job = ServerJob.from_dict(
                    message_dict["args"][0], RequestMetricsInput, DesignMetrics
                )
                server_job.status = ServerJobStatus.QUEUED()
                outgoing_message = ClientWebsocketIncoming.RECEIVEDMETRICSJOB(
                    server_job
                )

                ws.send(outgoing_message.to_json())
                server_job.status = ServerJobStatus.INPROGRESS()
                outgoing_message = ClientWebsocketIncoming.RECEIVEDMETRICSJOB(
                    server_job
                )
                ws.send(outgoing_message.to_json())
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
