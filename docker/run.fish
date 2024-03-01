#!/usr/bin/env fish

function rmImg
  set -l img (docker image ls --all --quiet --filter=dangling=true)
  if test -n "$img"
    docker image rm --force $img
  end
end

function rmCnt -a name
  set -l cnt (docker container ls --all --quiet --filter=name=$name)
  if test -n "$cnt"
    docker container rm --force $cnt
  end
end

function rmNet -a name
  set -l net (docker network ls --quiet --filter=name=$name)
  if test (count $net) -gt 1
    echo error: cannot remove more than one network, got $net >&2
    return 1
  end
  if test -n "$net"
    docker network rm --force $net
  end
end

function archInfo
  set -l img vlad/archinfo:0.1.0
  set -l cnt archinfo
  docker buildx build --tag $img .
  rmImg
  docker container run --name $cnt --rm $img
end

function caddyIndex
  set -l img vlad/caddy:0.1.0
  set -l cnt caddy
  set -l hostPort 5432
  docker buildx build --tag=$img .
  rmImg
  rmCnt $cnt
  docker container run --name $cnt --rm --detach --publish $hostPort:4321 $img
  docker container exec $cnt yq -p xml -o xml \
    -i '.html.body.h1 += " updated"' /opt/index.html
  curl http://localhost:$hostPort
end

function headClient -a version
  set -l img vlad/headclient:$version
  set -l cnt headclient
  set -l host https://github.com
  docker buildx build --tag $img .
  rmImg
  rmCnt $cnt
  docker container run --name $cnt --rm --detach --env HEAD_HOST=$host $img
  docker container logs --follow $cnt
end

function headClientToCaddy
  set -l net vladnet
  set -l srvImg vlad/caddy:0.1.0
  set -l clnImg vlad/headclient:0.2.0
  set -l srv caddy
  set -l cln headclient
  rmCnt $cln
  rmCnt $srv
  rmNet $net
  docker network create $net
  docker container run --name $srv --rm --detach --network $net $srvImg
  docker container run --name $cln --rm --detach \
    --env HEAD_HOST=http://$srv:4321 --network $net $clnImg
  docker container logs --follow $cln
end

# archInfo
# caddyIndex
# headClient 0.2.0
# headClientToCaddy

# rmCnt
