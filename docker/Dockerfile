# * buildBase
# FROM archlinux
# RUN pacman --noconfirm -Syyu && pacman --noconfirm -S fish bind go-yq go caddy

# * archInfo
# FROM vlad/base:0.1.0
# WORKDIR /opt
# CMD fish archinfo.fish
# COPY archinfo.fish .

# * caddyIndex
# FROM vlad/base:0.1.0
# CMD caddy run --config /opt/Caddyfile
# COPY Caddyfile index.html /opt

# * headClient
# FROM vlad/base:0.1.0 AS build
# WORKDIR /opt
# COPY go.mod go.sum headclient.go headclient.yaml .
# RUN go build
# FROM archlinux
# ENV HEADCLIENT_CONFIG=/opt/headclient.yaml
# WORKDIR /opt
# CMD ./headclient
# COPY --from=build /opt/headclient .
# COPY headclient.yaml .

# * healthCheck
# FROM vlad/base:0.1.0 AS build
# WORKDIR /opt
# COPY healthserver.go .
# RUN go build healthserver.go
# FROM archlinux
# WORKDIR /opt
# CMD ./healthserver
# HEALTHCHECK --interval=800ms \
#   CMD curl --fail http://healthserver:4321/health
# COPY --from=build /opt/healthserver .
