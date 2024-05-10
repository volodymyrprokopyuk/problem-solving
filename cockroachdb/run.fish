#!/usr/bin/env fish

set -g img cockroachdb/cockroach:latest-v23.2
set -g cnt cockroach
set -g cmd cockroach sql --insecure --format table

function server
  docker run --name $cnt --rm --detach --publish 26257:26257 \
    --volume crdb1:/cockroach/cockroach-data \
    --mount type=bind,source=(pwd),target=/cockroach/sql,readonly \
    $img start-single-node --insecure
end

function client
  docker exec -it $cnt $cmd
end

function query
  # docker exec $cnt $cmd --execute="SELECT 1 + 2 sum;"
  # echo "SELECT 1 + 2 sum;" | docker exec -i $cnt $cmd
  # docker exec $cnt $cmd --file sql/query.sql
  # cat query.sql | docker exec -i $cnt $cmd
  docker exec -i $cnt $cmd < query.sql
end

switch $argv[1]
case -s
  server
case -c
  client
case -q
  query
case '*'
  echo unknown option $argv[1]; return 1
end
