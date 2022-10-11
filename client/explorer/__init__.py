"""Initialize Flask app."""
from flask import Flask
from . import routes


def init_app():
    """Construct core Flask application."""
    app = Flask(__name__, instance_relative_config=False)
    app.config.from_object('config.Config')

    # register "blueprints"
    app.register_blueprint(routes.query_bp)

    return app
