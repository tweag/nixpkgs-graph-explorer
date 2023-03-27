# Nixpkgs graph explorer client

* This is a front-end client for the `nixpkgs-graph-explorer` project.
* The project includes a `Dockerfile` and two docker-compose files,
  `docker-compose.yml` and `docker-compose.prod.yml` for development and
  production deploys, respectively.
* The application expects configuration options to be supplied in environment
  variables prefixed with `FLASK_`. In this regard, two basic environment
  variable files are included in this repository: `.env.dev` and `.env.prod`.
  These are used by `docker-compose.yml` and `docker-compose.prod.yml`
  respectively.

Following is an explanation of how to set up various development and production
builds:


## Setting up a development environment with Docker

1. Ensure that `docker` and `docker-compose` are installed.
2. Optionally update environment variables in `.env.dev`
3. `docker-compose up` will build and run the `flask` development server

## Setting up a production environment with Docker

1. Ensure that `docker` and `docker-compose` are installed.
2. Update environment variables in `.env.prod`
3. `docker-compose -f docker-compose.prod.yml up` will build and run a production server using `gunicorn`
