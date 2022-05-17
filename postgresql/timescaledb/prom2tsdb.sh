#!/usr/bin/env zsh

set -eu

PROM_HOST=http://localhost:9090
PROM_QUERY_URL=/api/v1/query

PG_HOST=localhost
PG_USER=postgres
PG_DATABASE=timeseries

METRIC_END_TIME=2022-04-27T03:00:00Z
METRIC_BACK_INTERVAL=30s

function migrate_metric() {
  curl -sX POST $PROM_HOST$PROM_QUERY_URL \
      --data-urlencode "query=$FETCH_METRIC" \
      --data-urlencode "time=$METRIC_END_TIME" |
    jq -r $CONVERT_METRIC |
    psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE \
      -c $LOAD_METRIC -c $TRANSFORM_METRIC \
      -Xq -v ON_ERROR_STOP=on -P linestyle=unicode
}

# Node CPU utilization
FETCH_METRIC='node_cpu_seconds_total
  {instance=~"mongodb.+", cpu="0"}'"[$METRIC_BACK_INTERVAL]"
CONVERT_METRIC='.data.result[] | .metric as $metric | .values[] |
  [(.[0] | strftime("%Y-%m-%dT%H:%M:%SZ")),
    $metric.job, $metric.instance, $metric.cpu, $metric.mode, .[1]] | @csv'
LOAD_METRIC="\COPY node_cpu_utilization_long FROM stdin
  WITH (FORMAT csv, HEADER false, DELIMITER ',')"
TRANSFORM_METRIC="SELECT transform_node_cpu_utilization();"
migrate_metric
