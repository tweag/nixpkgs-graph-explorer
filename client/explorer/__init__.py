"""Initialize Flask app."""
from flask import Flask
from . import routes


def init_app():
    app = Flask(__name__, instance_relative_config=True)
    # the defaults in config.py
    app.config.from_object('config.Default')
    # the current instance settings
    app.config.from_pyfile('application.cfg', silent=True)
    # additional env variables prefixed with `FLASK_`
    app.config.from_prefixed_env()

    # register "blueprints"
    app.register_blueprint(routes.query_bp)

    return app
