from flask import Flask
from flask_cors import CORS
from flask_graphql import GraphQLView

from .big_structure_models import big_structure_db_session
from .design_models import designs_db_session

from .schema import schema

app = Flask(__name__)
CORS(app)
app.debug = True

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
