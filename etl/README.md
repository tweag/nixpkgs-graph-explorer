# etl

Scripts for populating a Gremlin database with nixpkgs graph data.

## Installation

Ensure python 3 is installed then install the required python dependencies with
something like:

```
pip install -r etl/requirements.txt
```

And then run the script:

```
python etl/example_query.py
```

## Docker

You can use docker to run the etl script:

```bash
cd etl
docker build -t etl .
docker run --rm -it --name=etl --network=host etl python etl.py
```
