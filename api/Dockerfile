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
COPY pyproject.toml pyproject.toml
COPY poetry.lock poetry.lock
RUN poetry export -f requirements.txt | /venv/bin/pip install -r /dev/stdin

# Install rest of this package
COPY . .
RUN poetry build && /venv/bin/pip install dist/*.whl
