#!/usr/bin/env bash

set -eu

readonly ROOT_DIR=$(pwd)
export PGDATABASE=playground
export PGUSER=vlad

function playground_schema {
    psql -c "DROP DATABASE IF EXISTS $PGDATABASE WITH (FORCE);" \
         -c "CREATE DATABASE $PGDATABASE WITH OWNER $PGUSER;" postgres
    psql -f $ROOT_DIR/playground-schema.sql
}

function playground_data {
    psql -f $ROOT_DIR/playground-data.sql
}

function playground_query {
    psql -f $ROOT_DIR/playground-query.sql
}

case $1 in
    -s|--scheme)
        playground_schema
        exit
        ;;
    -d|--data)
        playground_data
        exit
        ;;
    -q|--query)
        playground_query
        exit
        ;;
    *)
        echo "ERROR: unknown option $1"
        exit 1
        ;;
esac
