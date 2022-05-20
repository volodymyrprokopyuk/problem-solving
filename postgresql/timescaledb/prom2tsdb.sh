#!/usr/bin/env zsh

set -eu

PROM_HOST=http://localhost:9090
PROM_QUERY_URL=/api/v1/query

PG_HOST=localhost
PG_USER=postgres
PG_DATABASE=timeseries

METRIC_END_TIME=2022-04-27T03:25:00Z
METRIC_BACK_INTERVAL=50m

function metric_migrate() {
  curl -sX POST $PROM_HOST$PROM_QUERY_URL \
      --data-urlencode "query=$METRIC_FETCH" \
      --data-urlencode "time=$METRIC_END_TIME" |
    jq -r $METRIC_CONVERT |
    psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE \
      -c $METRIC_LOAD -c $METRIC_TRANSFORM \
      -Xq -v ON_ERROR_STOP=on -P linestyle=unicode
}

# Node CPU utilization
METRIC_FETCH='node_cpu_seconds_total'"[$METRIC_BACK_INTERVAL]"
METRIC_CONVERT='.data.result[] | .metric as $metric | .values[] |
  [(.[0] | strftime("%Y-%m-%dT%H:%M:%SZ")),
    $metric.job, $metric.instance, $metric.cpu, $metric.mode, .[1]] | @csv'
METRIC_LOAD="\COPY node_cpu_utilization_long FROM stdin
  WITH (FORMAT csv, HEADER false, DELIMITER ',')"
METRIC_TRANSFORM="SELECT transform_node_cpu_utilization();"
metric_migrate
