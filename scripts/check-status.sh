#!/bin/bash

# Helper script for checking the health of the Gremlin Server container
# in the nixpkgs-graph-explorer Docker Compose stack.

set -ex

# Note: The domain name and port below should match the Gremlin service name in 
# the docker-compose file.

response=$(curl --request POST \
     --data '{"gremlin": "g.V().limit(1).count()"}' \
     gremlin:8182/gremlin/status)

jq --exit-status '.status.code == 200' <<< "$response"
