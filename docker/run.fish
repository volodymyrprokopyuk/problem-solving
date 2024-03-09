#!/usr/bin/env fish

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

function rmVol -a name
  set -l vol (docker volume ls --quiet --filter name=$name)
  if test -n "$vol"
    docker volume rm --force $vol
  end
end

function buildBase
  set -l img vlad/base:0.1.0
  docker buildx build --tag $img .
  rmImg
end

function archInfo
  set -l img vlad/base:0.1.0
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

function headClient
  set -l img vlad/headclient:0.1.0
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
  set -l clnImg vlad/headclient:0.1.0
  set -l srv caddy
  set -l cln headclient
  rmCnt $cln
  rmCnt $srv
  rmNet $net
  docker network create $net
  docker container run --name $srv --rm --detach --network $net $srvImg
  docker container run --name $cln --rm --detach \
    --env HEADCLIENT_HOST=http://$srv:4321 --network $net $clnImg
  docker container logs --follow $cln
end

function randomFiles
  set -l img vlad/base:0.1.0
  set -l cnts rand1 rand2
  set -l cmd 'tr -dc a-z0-9 </dev/urandom | head -c 4 > /opt/rand.txt'
  for cnt in $cnts
    docker container run --name $cnt $img fish --command "$cmd"
    docker container cp $cnt:/opt/rand.txt $cnt.txt
    printf '%s: %s\n' $cnt.txt (cat $cnt.txt); rm -f $cnt.txt
    rmCnt $cnt
  end
end

function reuseVolume
  set -l vol appendvol
  set -l mnt /opt/vol
  set -l img vlad/base:0.1.0
  set -l cnt arch
  set -l cmd 'echo ok >> /opt/vol/out; echo; cat /opt/vol/out'
  docker volume create $vol
  for i in 1 2 3
    docker container run --name $cnt --rm --volume $vol:$mnt $img \
      fish --command "$cmd"
  end
  rmVol $vol
end

function bidiMount
  set -l img vlad/base:0.1.0
  set -l cnt arch
  set -l wCmd 'echo \* from container > /opt/out.txt'
  set -l rCmd 'cat /etc/passwd; cat /etc/index.html'
  docker container run --name $cnt --rm \
    --mount type=bind,source=(pwd),target=/opt,readonly $img \
    cat /opt/index.html
  docker container run --name $cnt --rm \
    --mount type=bind,source=(pwd),target=/opt $img fish --command "$wCmd"
  cat out.txt; rm -f out.txt
  docker container run --name $cnt --rm \
    --mount type=bind,source=(pwd)/index.html,target=/etc/index.html,readonly \
    $img bash -c "$rCmd"
end

function scaleClientDNS
  set -l srv docker-caddy-1
  set -l cmd 'nslookup caddy; nslookup headclient'
  docker-compose up --detach --scale headclient=2
  docker container exec $srv fish --command "$cmd"
  docker-compose down
end

function healthCheck
  set -l img vlad/healthserver:0.1.0
  set -l cnt healthserver
  docker buildx build --tag $img .
  rmImg
  rmCnt $cnt
  docker container run --name $cnt --rm --detach $img
  sleep 2s
  docker container ls
  sleep 2s
  docker container ls
end

# buildBase
# archInfo
# caddyIndex
# headClient
# headClientToCaddy
# randomFiles
# reuseVolume
# bidiMount
scaleClientDNS
# healthCheck
