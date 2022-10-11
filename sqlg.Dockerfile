##############################################################################
# Stage for fetching jars, etc.
##############################################################################
FROM alpine:3.16.2 AS builder

RUN apk update && apk add wget

# Download Gremlin Server
ARG TINKERPOP_VERSION=3.5.1
RUN mkdir /gremlin-server
WORKDIR /gremlin-server
RUN wget -O gremlin-server.zip "https://archive.apache.org/dist/tinkerpop/${TINKERPOP_VERSION}/apache-tinkerpop-gremlin-server-${TINKERPOP_VERSION}-bin.zip" \
    && unzip gremlin-server.zip
RUN mv "apache-tinkerpop-gremlin-server-${TINKERPOP_VERSION}" gremlin-server

##############################################################################
# Stage containing the final image
##############################################################################
FROM eclipse-temurin:11-jre-alpine

# Runtime deps
RUN apk update && apk add bash perl

# Create user
RUN addgroup -S gremlin && adduser -S gremlin -G gremlin

# Install Gremlin Server
COPY --from=builder --chown=gremlin:gremlin /gremlin-server/gremlin-server /gremlin-server

# Install additional jars we downloaded in previous stage
WORKDIR /gremlin-server
# Ensure permissions are properly set
RUN chmod -R 700 /gremlin-server

# Enable gremlinpython support
RUN ./bin/gremlin-server.sh install "org.apache.tinkerpop" "gremlin-python" "3.4.13"

# Enable sqlg support
ARG SQLG_VERSION=2.1.6

RUN ./bin/gremlin-server.sh install "org.umlg" "sqlg-hikari" "${SQLG_VERSION}"
RUN ./bin/gremlin-server.sh install "org.umlg" "sqlg-postgres" "${SQLG_VERSION}"

RUN mkdir /config && chown -R gremlin:gremlin /config
RUN chmod -R u+rw /config

# Bake in our required configuration
COPY --chown=gremlin:gremlin ./config/gremlin-server.yaml /config
COPY --chown=gremlin:gremlin ./config/gremlin-server-sqlg-postgres.groovy /config

# Switch to application user
USER gremlin

# Set default gremlin server config to the one provided above
ENV GREMLIN_YAML=/config/gremlin-server.yaml

COPY ./docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT "/docker-entrypoint.sh"

CMD /gremlin-server/bin/gremlin-server.sh console
