"""Application entry point."""
from explorer import app, gremlin_client


def run_app():
    try:
        app.run(host="0.0.0.0")
    finally:
        gremlin_client.close()


if __name__ == "__main__":
    run_app()
