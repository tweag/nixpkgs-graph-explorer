# syntax=docker/dockerfile:1
FROM python:3.10.7
WORKDIR /code

# set environment variables
ENV FLASK_APP wsgi.py
ENV FLASK_RUN_HOST 0.0.0.0
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1

# if you use an alpine python image, the following is necessary
# RUN apk add --no-cache gcc musl-dev linux-headers
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY . .
