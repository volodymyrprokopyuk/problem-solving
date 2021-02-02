#!/usr/bin/env bash

set -eu

readonly ROOT_DIR=$(pwd)
# export PGDATABASE=playground
# export PGDATABASE=booking
export PGDATABASE=factbook
export PGUSER=vlad

function create_schema {
    psql -c "DROP DATABASE IF EXISTS $PGDATABASE WITH (FORCE);" \
         -c "CREATE DATABASE $PGDATABASE WITH OWNER $PGUSER;" postgres
    psql -f $ROOT_DIR/$PGDATABASE/schema.sql
}

function load_data {
    psql -f $ROOT_DIR/$PGDATABASE/data.sql
}

function execute_query {
    psql -f $ROOT_DIR/$PGDATABASE/query.sql
}

case $1 in
    -s|--scheme)
        create_schema
        exit
        ;;
    -d|--data)
        load_data
        exit
        ;;
    -q|--query)
        execute_query
        exit
        ;;
    *)
        echo "ERROR: unknown option $1"
        exit 1
        ;;
esac
