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

# * headClient 0.1.0
# FROM archlinux
# ENV HEAD_HOST=https://google.com
# WORKDIR /opt
# CMD ./headclient
# COPY headclient .

# * headClient 0.2.0
# FROM vlad/base:0.1.0 AS build
# WORKDIR /opt
# COPY headclient.go .
# RUN go build headclient.go
# FROM archlinux
# ENV HEAD_HOST=https://google.com
# WORKDIR /opt
# CMD ./headclient
# COPY --from=build /opt/headclient .

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
