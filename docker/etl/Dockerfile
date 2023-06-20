# syntax=docker/dockerfile:1
FROM python:3.10.7
WORKDIR /code

# install nix for data extraction
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux --init none --no-confirm

# set environment variables
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
ENV POETRY_VIRTUALENVS_CREATE=0
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"
ENV PATH="/venv/bin:${PATH}"
ENV VIRTUAL_ENV="/venv"

# if you use an alpine python image, the following is necessary
# RUN apk add --no-cache gcc musl-dev linux-headers
# Note: This version of poetry must match what is in the `flake.nix`
RUN pip install "poetry==1.5.1"

RUN python -m venv /venv

# install dependencies first for better caching
RUN mkdir -p ./build/api
ADD ./api/pyproject.toml ./api/poetry.lock build/api
RUN mkdir -p ./build/core
COPY ./core ./build/core
WORKDIR /code/build/api
RUN poetry install --only main --no-root

# build and install local packages from this repository

# build and install local dependencies of api

# core
WORKDIR /code/build/core
RUN poetry build && /venv/bin/pip install dist/*.whl

# build and install api itself
WORKDIR /code/build/api
COPY ./api .
RUN poetry build && /venv/bin/pip install dist/*.whl

# set the entrypoint to run the ETL commands
ENTRYPOINT ["sh", "-c", "python -m explorer.extract derivations.jsonl && python -m explorer.api.ingest derivations.jsonl"]
