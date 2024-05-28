#!/usr/bin/env fish

set -g img bitnami/kafka:3.7.0
set -g cnt kafka
set -g srv --bootstrap-server localhost:9092

function server
  docker run --name $cnt --hostname $cnt --detach --rm \
    -e KAFKA_CFG_NODE_ID=0 \
    -e KAFKA_CFG_PROCESS_ROLES=controller,broker \
    -e KAFKA_CFG_LISTENERS=PLAINTEXT://:9092,CONTROLLER://:9093 \
    -e KAFKA_CFG_LISTENER_SECURITY_PROTOCOL_MAP=CONTROLLER:PLAINTEXT,PLAINTEXT:PLAINTEXT \
    -e KAFKA_CFG_CONTROLLER_QUORUM_VOTERS=0@kafka:9093 \
    -e KAFKA_CFG_CONTROLLER_LISTENER_NAMES=CONTROLLER \
    $img
end

function client
  docker exec -it $cnt kafka-topics.sh $srv --list
end

switch $argv[1]
case -s
  server
case -c
  client
case '*'
  echo unknown option $argv[1]; return 1
end
