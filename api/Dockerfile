# syntax=docker/dockerfile:1
FROM python:3.10.7
WORKDIR /code

# set environment variables
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1

# if you use an alpine python image, the following is necessary
# RUN apk add --no-cache gcc musl-dev linux-headers
# Note: This version of poetry must match what is in the `flake.nix`
RUN pip install "poetry==1.3.2"

RUN python -m venv /venv

# Install dependencies first for better caching
RUN mkdir -p ./build/api
COPY api/pyproject.toml build/api/pyproject.toml
COPY api/poetry.lock build/api/poetry.lock
RUN poetry --directory build/api export -f requirements.txt | /venv/bin/pip install -r /dev/stdin

## Build and install local packages from this repository

# Build and install local dependencies of api

# core
RUN mkdir -p ./build/core 
WORKDIR /code/build/core
COPY ./core .
RUN poetry build && /venv/bin/pip install dist/*.whl

# Build and install api itself
WORKDIR /code/build/api
COPY ./api .
RUN poetry build && /venv/bin/pip install dist/*.whl
