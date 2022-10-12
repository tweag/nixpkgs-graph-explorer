# nixpkgs-graph-explorer

## Running the graph database locally

The database back-end is currently configured with sqlg. It is designed to be accessed via the Gremlin Server instance provided in the Docker Compose file.

To run the Docker Compose stack, use:

```
docker-compose up
```

Then, to run some test Gremlin queries, ensure python 3 is installed then install the required python dependencies with something like:

```
pip install -r etl/requirements.txt
```

And then run the script:

```
python etl/example_query.py
```
