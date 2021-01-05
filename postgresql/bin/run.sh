#!/usr/bin/env bash

set -eu

readonly ROOT_DIR=$(pwd)
export PGUSER=vlad
export PGDATABASE=playgournd

function playground_schema {
    psql -c "DROP DATABASE IF EXISTS $PGDATABASE WITH (FORCE);" \
         -v ON_ERROR_STOP=1 -v ECHO=queries postgres
    psql -c "CREATE DATABASE $PGDATABASE WITH OWNER $PGUSER;" \
         -v ON_ERROR_STOP=1 -v ECHO=queries postgres
    psql -f $ROOT_DIR/playground-schema.sql \
         -v ON_ERROR_STOP=1 -v ECHO=queries
}

function playground_data {
    psql -f $ROOT_DIR/playground-data.sql \
         -v ON_ERROR_STOP=1 -v ECHO=queries
}

function playground_query {
    psql -f $ROOT_DIR/playground-query.sql \
         -v ON_ERROR_STOP=1 -v ECHO=queries
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
