# docker buildx build --tag vlad/archinfo:0.1.0 .
# docker container run --name archinfo --rm vlad/archinfo:0.1.0
# FROM archlinux
# WORKDIR /opt
# CMD ./archinfo.sh
# COPY archinfo.sh /opt

# docker buildx build --tag vlad/caddy:0.1.0 .
# docker container run --name caddy --rm -d -p 5432:4321 vlad/caddy:0.1.0
# docker container exec -it caddy yq -p xml -o xml \
#   -i '.html.body.h1 = "Caddy from Docker 2"' /opt/index.html
# FROM archlinux
# RUN pacman --noconfirm -Syyu && pacman --noconfirm -S caddy go-yq
# CMD caddy run --config /opt/Caddyfile
# COPY Caddyfile index.html /opt

# docker buildx build --tag vlad/headclient:0.1.0 .
# docker container run --name headclient --rm -d \
#   -e HEAD_HOST=https://github.com vlad/headclient:0.1.0
# docker container logs --follow headclient
# FROM archlinux
# ENV HEAD_HOST=https://google.com
# WORKDIR /opt
# CMD ./headclient
# COPY headclient /opt

# docker buildx build --tag vlad/headclient:0.2.0 .
# docker container run --name headclient --rm -d \
#   -e HEAD_HOST=https://github.com vlad/headclient:0.2.0
# docker container logs --follow headclient
# FROM archlinux AS build
# RUN pacman --noconfirm -Syyu && pacman --noconfirm -S go
# WORKDIR /opt
# COPY headclient.go /opt
# RUN go build headclient.go
# FROM archlinux
# ENV HEAD_HOST=https://google.com
# WORKDIR /opt
# CMD ./headclient
# COPY --from=build /opt/headclient /opt

# docker network crate vladnet
# docker container run --name caddy --rm -d --network vladnet vlad/caddy:0.1.0
# docker container run --name headclient --rm -d \
#   -e HEAD_HOST=http://caddy:4321 --network vladnet vlad/headclient:0.2.0
# docker container logs --follow headclient
