FROM node:19-alpine AS builder

WORKDIR /app

ADD . .

RUN npm ci
RUN npm run build


FROM caddy:2.6-alpine

COPY --from=builder /app/dist /usr/share/caddy
COPY Caddyfile /etc/caddy/Caddyfile
