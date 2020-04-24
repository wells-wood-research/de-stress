from flask import Flask
from flask_cors import CORS
from flask_graphql import GraphQLView
from flask_sockets import Sockets

from .big_structure_models import big_structure_db_session
from .design_models import designs_db_session

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
        message = ws.receive()
        ws.send(message)


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
