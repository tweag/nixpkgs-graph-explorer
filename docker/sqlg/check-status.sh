#!/bin/bash

set -ex

curl --request POST \
     --data '{"gremlin": "g.V().limit(1).count()"}' \
     localhost:8182/gremlin/status | jq --exit-status '.status.code == 200'
