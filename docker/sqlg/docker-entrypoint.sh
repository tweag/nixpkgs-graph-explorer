#!/bin/bash

# Adapted from the entrypoint for the main gremlin-server Docker image

set -ex

if [[ -z "${GREMLIN_YAML}" ]]; then
  echo '$GREMLIN_YAML environment variable was not set. This variable must be set with the path to a gremlin server yaml config file.'
else
  export CONF_FILE="${GREMLIN_YAML}"
fi

# Note: because of the intricacies of Gremlin Server, we have to update the gremlin server config
# file with the ip address assigned to the container by Docker........

# IP substitution hack borrowed from:
# https://github.com/htaox/NEAT/blob/94a004831cf89767e116d955192fc14ac82e5069/docker-scripts/gremlin-server-3.0.0/files/default_cmd#L5
IP=$(ip -o -4 addr list eth0 | perl -n -e 'if (m{inet\s([\d\.]+)\/\d+\s}xms) { print $1 }')
sed -i "s|^host:.*|host: $IP|" $CONF_FILE

/gremlin-server/bin/gremlin-server.sh console
