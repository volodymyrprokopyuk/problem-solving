# * archInfo
# FROM archlinux
# WORKDIR /opt
# CMD ./archinfo.sh
# COPY archinfo.sh .
# RUN chmod 755 archinfo.sh

# * caddyIndex
# FROM archlinux
# RUN pacman --noconfirm -Syyu && pacman --noconfirm -S caddy bind go-yq
# CMD caddy run --config /opt/Caddyfile
# COPY Caddyfile index.html /opt

# * headClient 0.1.0
# FROM archlinux
# ENV HEAD_HOST=https://google.com
# WORKDIR /opt
# CMD ./headclient
# COPY headclient .

# * headClient 0.2.0
# FROM archlinux AS build
# RUN pacman --noconfirm -Syyu && pacman --noconfirm -S go
# WORKDIR /opt
# COPY headclient.go .
# RUN go build headclient.go
# FROM archlinux
# ENV HEAD_HOST=https://google.com
# WORKDIR /opt
# CMD ./headclient
# COPY --from=build /opt/headclient .
