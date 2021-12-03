#!/usr/bin/env zsh

set -eu

CONTENT_JSON='Content-Type: application/json; charset=UTF-8'
CONTENT_FORM='Content-Type: application/x-www-form-urlencoded'

DOCKER_NETWORK=ods

HYDRA_IMAGE=oryd/hydra:v1.10.7-sqlite
HYDRA_CONFIG=$(pwd)/tm
HYDRA_NAME=hydra
HYDRA_PUBLIC_URL=https://localhost:4444
HYDRA_ADMIN_URL=https://localhost:4445

OATH_IMAGE=oryd/oathkeeper:v0.38.15-alpine
OATH_CONFIG=$(pwd)/iap
OATH_NAME=oathkeeper
OATH_PROXY_URL=https://localhost:4455
OATH_ADMIN_URL=https://localhost:4456

DS_IMAGE=ods/ds:v0.1.0
DS_CONFIG=$(pwd)/ds
DS_NAME=ds
DS_URL=http://localhost:4477

function docker_build_image {
  image=${1?"Docker image tag is mandatory"}
  dockerfile=${2?"Dockerfile is mandatory"}
  docker build -t $image $dockerfile
}

function docker_create_network {
  network=${1?"Docker network is mandatory"}
  network_id=$(docker network ls -qf "name=^$network$")
  [[ -n $network_id ]] || docker network create $network
  docker network ls -f "name=^$network$"
}

function docker_get_container_id {
  name=${1?"Container name is mandatory"}
  docker ps -qf "name=^$name$"
}

function tls_generate_key_pair {
  common_name=${1?"Common name is mandatory"}
  key_path=${2?"Key pair path is mandatory"}
  [[ -e $key_path/cert.pem ]] || \
    openssl req -x509 -newkey rsa:4096 -sha256 -nodes \
      -subj "/CN=$common_name" -days 365 \
      -addext "subjectAltName=DNS:localhost,DNS:$common_name" \
      -out $key_path/cert.pem -keyout $key_path/key.pem && \
      chmod 644 $key_path/key.pem
  openssl x509 -noout -text -in $key_path/cert.pem | \
    grep 'Subject:\|DNS:'
}

function jwks_generate_key_pair {
  jwks_file=${1?"JWKS file is mandatory"}
  [[ -e $jwks_file ]] || \
    rnbyc -j -g RSA4096 -a RS256 |
      jq '.keys[0].kid = ("private:" + .keys[0].kid) |
      .keys[1].kid = ("public:" + .keys[1].kid) |
      .keys[].use = "sig"' > $jwks_file
  jq -r '.keys[].kid' $jwks_file
}

function hydra_start {
  network=${1?"Docker network is mandatory"}
  image=${2?"Hydra image is mandatory"}
  config_src=${3?"Hydra config is mandatory"}
  config_dst=/hydra
  name=${4?"Hydra name is mandatory"}
  docker run --rm -it --name $name --network $network \
    --mount type=bind,src=$config_src,dst=$config_dst,readonly \
    $image serve all -c $config_dst/hydra_config.yaml
}

function alpine_import_certificate {
  container_id=${1?"Container id is mandatory"}
  cert_src=${2?"Certificate source is mandatory"}
  cert_dst=/etc/ssl/certs
  cert_dst_name=${3?"Certificate destination name is mandatory"}
  docker exec -i -u 0 $container_id \
    sh -c "cat > $cert_dst/$cert_dst_name" < $cert_src
}

function hydra_import_jwks {
  jwks_file=${1?"Hydra JWKS file is mandatory"}
  admin_url=${2?"Hydra admin URL is mandatory"}
  key_set=${3?"Hydra JWKS is mandatory"}
  key_set_url=$admin_url/keys/$key_set
  curl -kX PUT $key_set_url -H $CONTENT_JSON -d @$jwks_file > /dev/null
  curl -kX GET $key_set_url | jq -r '.keys[].kid'
}

function hydra_import_clients {
  clients=${1?"Hydra clients are mandatory"}
  admin_url=${2?"Hydra admin URL is mandatory"}
  clients_url=$admin_url/clients
  for client in $~clients; do
    curl -kX POST $clients_url -H $CONTENT_JSON -d @$client > /dev/null
    curl -kX GET $clients_url/$(jq -r '.client_id' $client) | jq .
  done
}

function hydra_get_token {
  public_url=${1?"Hydra public URL is mandatory"}
  client=${2?"Client is mandatory"}
  audience=${3?"Audience is mandatory"}
  scope=${4?"Scope is mandatory"}
  curl -kX POST $public_url/oauth2/token -H $CONTENT_FORM \
    -d client_id=$(jq -r '.client_id' $client) \
    -d client_secret=$(jq -r '.client_secret' $client) \
    -d grant_type=client_credentials \
    -d audience=$audience \
    -d scope=$scope
}

function hydra_introspect_token {
  admin_url=${1?"Admin URL is mandatory"}
  token=${2?"Token is mandatory"}
  curl -kX POST $admin_url/oauth2/introspect -H $CONTENT_FORM \
    -d token=$(jq -r '.access_token' <<<$token)
}

function jwt_decode_token {
  jwt=${1?"JWT is mandatory"}
  jwks_url=${2?"JWKS URL is mandatory"}
  rnbyc -t $jwt -H -C -P $(curl -ksX GET $jwks_url)
}

function oathkeeper_start {
  network=${1?"Docker network is mandatory"}
  image=${2?"Oathkeeper image is mandatory"}
  config_src=${3?"Oathkeeper config is mandatory"}
  config_dst=/oathkeeper
  name=${4?"Oathkeeper name is mandatory"}
  docker run --rm -it --name $name --network $network \
    --mount type=bind,src=$config_src,dst=$config_dst,readonly \
    -p 4455:4455 \
    $image serve -c $config_dst/oathkeeper_config.yaml
}

function ds_start {
  network=${1?"Docker network is mandatory"}
  image=${2?"DS image is mandatory"}
  config_src=${3?"DS config is mandatory"}
  config_dst=/opt/ds
  name=${4?"DS name is mandatory"}
  docker run --rm -it --name $name --network $network \
    --mount type=bind,src=$config_src,dst=$config_dst,readonly \
    $image $config_dst/ds_config.yaml
}

function arch_import_certificate {
  container_id=${1?"Container id is mandatory"}
  cert_src=${2?"Certificate source is mandatory"}
  cert_dst=/etc/ca-certificates/trust-source/anchors
  cert_dst_name=${3?"Certificate destination name is mandatory"}
  docker exec -i $container_id \
    sh -c "cat > $cert_dst/$cert_dst_name" < $cert_src
  docker exec -it $container_id sh -c 'update-ca-trust'
}

function ds_call {
  method=${1?"DS method is mandatory"}
  url=${2?"DS URL is mandatory"}
  token=$3
  curl -kX $method $url -H "Authorization: Bearer $token"
}

case $1 in
  -i|--init)
    docker_build_image $DS_IMAGE $DS_CONFIG
    docker_create_network $DOCKER_NETWORK
    tls_generate_key_pair $HYDRA_NAME $HYDRA_CONFIG
    tls_generate_key_pair $OATH_NAME $OATH_CONFIG
    jwks_generate_key_pair $HYDRA_CONFIG/access_token.jwks
    ;;
  -h|--hydra)
    hydra_start $DOCKER_NETWORK $HYDRA_IMAGE $HYDRA_CONFIG $HYDRA_NAME
    ;;
  -hc|--hydra-certificate)
    HYDRA_ID=$(docker_get_container_id $HYDRA_NAME)
    alpine_import_certificate $HYDRA_ID $HYDRA_CONFIG/cert.pem cert_hydra.pem
    ;;
  -hk|--hydra-jwks)
    hydra_import_jwks $HYDRA_CONFIG/access_token.jwks $OATH_PROXY_URL \
      hydra.jwt.access-token
    ;;
  -c|--clients)
    hydra_import_clients "$HYDRA_CONFIG/client_*.json" $OATH_PROXY_URL
    ;;
  -o|--oath)
    oathkeeper_start $DOCKER_NETWORK $OATH_IMAGE $OATH_CONFIG $OATH_NAME
    ;;
  -oc|--oath-certiticate)
    OATH_ID=$(docker_get_container_id $OATH_NAME)
    alpine_import_certificate $OATH_ID $HYDRA_CONFIG/cert.pem cert_hydra.pem
    ;;
  -d|--ds)
    ds_start $DOCKER_NETWORK $DS_IMAGE $DS_CONFIG $DS_NAME
    ;;
  -dc|--ds-certificate)
    DS_ID=$(docker_get_container_id $DS_NAME)
    arch_import_certificate $DS_ID $HYDRA_CONFIG/cert.pem cert_hydra.pem
    ;;
  -t|--token)
    OAUTH2_TOKEN=$(hydra_get_token $OATH_PROXY_URL $(pwd)/tm/client_ipf.json \
      ods_inquiry "read_public read_confidential")
    jq . <<<$OAUTH2_TOKEN
    hydra_introspect_token $OATH_PROXY_URL $OAUTH2_TOKEN | jq .
    ds_call GET $OATH_PROXY_URL/opaque-introspection \
      $(jq -r '.access_token' <<<$OAUTH2_TOKEN) | jq .
    ;;
  -j|--jwt)
    OAUTH2_JWT=$(hydra_get_token $OATH_PROXY_URL $(pwd)/tm/client_ipf.json \
      ods_inquiry "read_public read_confidential")
    jq . <<<$OAUTH2_JWT
    hydra_introspect_token $OATH_PROXY_URL $OAUTH2_JWT | jq .
    jwt_decode_token $(jq -r '.access_token' <<<$OAUTH2_JWT) \
      $OATH_PROXY_URL/.well-known/jwks.json | jq .
    ds_call GET $OATH_PROXY_URL/jwt-verification \
      $(jq -r '.access_token' <<<$OAUTH2_JWT) | jq .
    ;;
  *)
    exit 1
    ;;
esac
