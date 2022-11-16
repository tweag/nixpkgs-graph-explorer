"""Application entry point."""
from explorer import init_app


app, gremlin_client = init_app()

if __name__ == "__main__":
    try:
        app.run(host="0.0.0.0")
    finally:
        gremlin_client.close()

