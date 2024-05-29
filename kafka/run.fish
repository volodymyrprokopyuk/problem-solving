#!/usr/bin/env fish

# set -g img bitnami/kafka:3.7.0
# set -g cnt kafka
# set -g srv --bootstrap-server localhost:9092

# function server
#   docker run --name $cnt --hostname $cnt --detach --rm \
#     -e KAFKA_CFG_NODE_ID=0 \
#     -e KAFKA_CFG_PROCESS_ROLES=controller,broker \
#     -e KAFKA_CFG_LISTENERS=PLAINTEXT://:9092,CONTROLLER://:9093 \
#     -e KAFKA_CFG_LISTENER_SECURITY_PROTOCOL_MAP=CONTROLLER:PLAINTEXT,PLAINTEXT:PLAINTEXT \
#     -e KAFKA_CFG_CONTROLLER_QUORUM_VOTERS=0@kafka:9093 \
#     -e KAFKA_CFG_CONTROLLER_LISTENER_NAMES=CONTROLLER \
#     $img
# end

# function client
#   docker exec -it $cnt kafka-topics.sh $srv --list
# end

set -g img redpandadata/redpanda:v24.1.3
set -g cnt redpanda
set -g intz internal://0.0.0.0
set -g extz external://0.0.0.0
set -g inth internal://$cnt
set -g exth external://localhost
set -g kafka 9092 19092
set -g proxy 8082 18082
set -g registry 8081 18081
set -g rpc 33145

function server
  docker run --name $cnt --hostname $cnt --rm \
    $img redpanda start \
    --kafka-addr $intz:$kafka[1],$extz:$kafka[2] \
    --advertise-kafka-addr $inth:$kafka[1],$exth:$kafka[2] \
    --pandaproxy-addr $intz:$proxy[1],$extz:$proxy[2] \
    --advertise-pandaproxy-addr $inth:$proxy[1],$exth:$proxy[2] \
    --schema-registry-addr $intz:$registry[1],$extz:$registry[2] \
    --rpc-addr $cnt:$rpc \
    --advertise-rpc-addr $cnt:$rpc \
    --mode dev-container --smp 1 --default-log-level=warn
end

function client
  set -l topic chat
  # docker exec -it $cnt rpk cluster info
  docker exec -it $cnt rpk topic create $topic
  # echo "Hi" | docker exec -i $cnt rpk topic produce $topic
  docker exec -i $cnt rpk topic produce $topic < (echo "Hi" | psub)
  docker exec -it $cnt rpk topic consume $topic --num 1
  docker exec -it $cnt rpk topic delete $topic
end

switch $argv[1]
case -s
  server
case -c
  client
case '*'
  echo unknown option $argv[1]; return 1
end
