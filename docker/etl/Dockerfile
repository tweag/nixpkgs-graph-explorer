# syntax=docker/dockerfile:1
FROM python:3.10.7-slim

# install nix for data extraction
RUN apt-get update && apt-get install -y curl
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux --init none --no-confirm

# set environment variables
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
ENV POETRY_VIRTUALENVS_CREATE=0
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"

# install poetry
RUN pip install "poetry==1.5.1"

# install dependencies
ADD ./core /code/core/
ADD ./api/pyproject.toml ./api/poetry.lock /code/api/
WORKDIR /code/api/
RUN poetry install --with "local" --without "dev" --no-root

# install api itself
ADD ./api /code/api/
RUN poetry install --only-root

# set the entrypoint to run the ETL commands
ENTRYPOINT ["sh", "-c", "python -m explorer.extract derivations.jsonl && python -m explorer.api.ingest derivations.jsonl"]
