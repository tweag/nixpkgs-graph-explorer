"""Initialize Flask app."""
from flask import Flask
from explorer.query_bp import query_bp


def init_app():
    app = Flask(__name__, instance_relative_config=True)
    # the defaults in config.py
    app.config.from_object('config.Default')
    # additional env variables prefixed with `FLASK_`
    app.config.from_prefixed_env()

    # register "blueprints"
    app.register_blueprint(query_bp, url_prefix='')

    return app
