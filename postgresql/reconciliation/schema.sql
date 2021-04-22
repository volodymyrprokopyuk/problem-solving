BEGIN;

CREATE SCHEMA rx;

SET search_path TO rx, public;

CREATE TABLE rx.external_tx (
    tx_id text PRIMARY KEY,
    tx_ts timestamptz NOT NULL CHECK (tx_ts < (current_timestamp + '3 days'::interval)),
    tx_amount numeric(14,2) CHECK (abs(tx_amount) < 1e12),
    tx_currency text NOT NULL CHECK (tx_currency ~ '^[A-Z]{3}$'),
    profile text NOT NULL CHECK (profile ~ '^.{3,}$'));

CREATE FUNCTION rx.load_external_tx(a_data text[], a_delimiter text = ';')
RETURNS TABLE (tx_id text)
LANGUAGE SQL AS $$
WITH tx AS (
    SELECT regexp_split_to_array(tx.line, ';') AS data
    FROM unnest(a_data) tx(line))
INSERT INTO external_tx (tx_id, tx_ts, tx_amount, tx_currency, profile)
SELECT t.data[1], t.data[2]::timestamptz, t.data[3]::numeric, t.data[4], t.data[5]
FROM tx t
RETURNING tx_id;
$$;

SELECT t.tx_id FROM load_external_tx(ARRAY[
'TX001;2021-04-21 08:09:10;100.00;GBP;PROF01',
'TX002;2021-04-21 09:10:11;-200.00;GBP;PROF01',
'TX003;2021-04-21 10:11:12;300.00;GBP;PROF01',
'TX004;2021-04-21 11:12:13;-400.00;GBP;PROF01',
'TX005;2021-04-21 12:13:14;500.00;EUR;PROF01',
'TX006;2021-04-21 13:14:15;-10.00;EUR;PROF01',
'TX010;2021-04-21 20:20:20;20.00;GBP;PROF01',
'TX100;2021-04-22 08:09:10;100.00;GBP;PROF01',
'TX101;2021-04-21 08:09:10;30.00;GBP;PROF02']) t;

TABLE external_tx;

CREATE TABLE rx.internal_tx (
    tx_id text PRIMARY KEY,
    tx_json jsonb NOT NULL CHECK (
        (tx_json->>'tx_ts')::date < (current_timestamp + '3 days'::interval)
        AND abs((tx_json->>'tx_amount')::numeric) < 1e12
        AND tx_json->>'tx_currency' ~ '^[A-Z]{3}$'
        AND tx_json->>'profile' ~ '^.{3,}$'));

CREATE FUNCTION rx.load_internal_tx(a_data text[])
RETURNS TABLE (tx_id text)
LANGUAGE SQL AS $$
WITH tx AS (
    SELECT tx.json::jsonb json
    FROM unnest(a_data) tx(json))
INSERT INTO internal_tx (tx_id, tx_json)
SELECT t.json->>'tx_id', t.json
FROM tx t
RETURNING tx_id;
$$;

SELECT t.tx_id FROM load_internal_tx(ARRAY[
'{"tx_id": "TX001", "tx_ts": "2021-04-21 08:09:10", "tx_amount": -100.00, "tx_currency": "GBP", "profile": "PROF01"}',
'{"tx_id": "TX002", "tx_ts": "2021-04-21 09:10:11", "tx_amount": 200.00, "tx_currency": "GBP", "profile": "PROF01"}',
'{"tx_id": "TX003", "tx_ts": "2021-04-21 10:11:12", "tx_amount": -300.00, "tx_currency": "GBP", "profile": "PROF01"}',
'{"tx_id": "TX004", "tx_ts": "2021-04-21 11:12:13", "tx_amount": 400.00, "tx_currency": "GBP", "profile": "PROF01"}',
'{"tx_id": "TX005", "tx_ts": "2021-04-21 12:13:14", "tx_amount": -500.00, "tx_currency": "EUR", "profile": "PROF01"}',
'{"tx_id": "TX006", "tx_ts": "2021-04-21 13:14:15", "tx_amount": 20.00, "tx_currency": "EUR", "profile": "PROF01"}',
'{"tx_id": "TX020", "tx_ts": "2021-04-21 20:20:20", "tx_amount": 30.00, "tx_currency": "GBP", "profile": "PROF01"}',
'{"tx_id": "TX100", "tx_ts": "2021-04-22 08:09:10", "tx_amount": -100.00, "tx_currency": "GBP", "profile": "PROF01"}',
'{"tx_id": "TX201", "tx_ts": "2021-04-21 08:09:10", "tx_amount": 40.00, "tx_currency": "GBP", "profile": "PROF02"}']) t;

TABLE internal_tx;

CREATE TYPE rx.rx_status_t AS ENUM ('RECONCILIATED', 'PP_MISSED', 'JPMC_MISSED');

CREATE FUNCTION rx.reconciliate_tx(a_date date[], a_profile text[], a_currency text[])
RETURNS TABLE (ext_tx_id text, int_tx_id text, rx_status rx_status_t)
LANGUAGE SQL AS $$
SELECT et.tx_id, it.tx_id, (CASE
    WHEN et.tx_id IS NULL THEN 'PP_MISSED'
    WHEN it.tx_id IS NULL THEN 'JPMC_MISSED'
    ELSE 'RECONCILIATED' END)::rx_status_t rx_status
FROM external_tx et
    FULL JOIN internal_tx it ON et.tx_id = it.tx_id
        AND et.tx_ts::date = (it.tx_json->>'tx_ts')::date
        AND et.profile = it.tx_json->>'profile'
        AND -et.tx_amount = (it.tx_json->>'tx_amount')::numeric
        AND et.tx_currency = it.tx_json->>'tx_currency'
WHERE (et.tx_ts::date = ANY(a_date) OR (it.tx_json->>'tx_ts')::date = ANY(a_date))
    AND (et.profile = ANY(a_profile) OR it.tx_json->>'profile' = ANY(a_profile))
    AND (et.tx_currency = ANY(a_currency) OR it.tx_json->>'tx_currency' = ANY(a_currency));
$$;

\set tx_date '{2021-04-21, 2021-04-22}'
\set tx_profile '{PROF01, PROF02}'
\set tx_currency '{GBP, EUR}'

SELECT r.ext_tx_id "PayPal", r.int_tx_id "JPMC", r.rx_status "RX status"
FROM reconciliate_tx(:'tx_date', :'tx_profile', :'tx_currency') r;

CREATE FUNCTION rx.summarize_rx(a_date date[], a_profile text[], a_currency text[])
RETURNS TABLE (
    ext_date date, ext_profile text, ext_currency text, ext_net_amount numeric,
    rx_status rx_status_t,
    int_net_amount numeric, int_currency text, int_profile text, int_date date)
LANGUAGE SQL AS $$
SELECT et.tx_ts::date ext_date,
    et.profile ext_profile,
    et.tx_currency ext_currency,
    sum(et.tx_amount) ext_net_amount,
    r.rx_status,
    sum((it.tx_json->>'tx_amount')::numeric) int_net_amount,
    it.tx_json->>'tx_currency' int_currency,
    it.tx_json->>'profile' int_profile,
    (it.tx_json->>'tx_ts')::date int_date
FROM reconciliate_tx(a_date, a_profile, a_currency) r
    LEFT JOIN external_tx et ON et.tx_id = r.ext_tx_id
    LEFT JOIN internal_tx it ON it.tx_id = r.int_tx_id
GROUP BY et.tx_ts::date, (it.tx_json->>'tx_ts')::date,
    et.profile, it.tx_json->>'profile',
    et.tx_currency, it.tx_json->>'tx_currency',
    r.rx_status
ORDER BY coalesce(et.tx_ts::date, (it.tx_json->>'tx_ts')::date),
    coalesce(et.profile, it.tx_json->>'profile'),
    coalesce(et.tx_currency, it.tx_json->>'tx_currency'),
    r.rx_status;
$$;

SELECT r.ext_date "PayPal date", r.ext_profile "PayPal profile",
    r.ext_currency "PayPal currency", r.ext_net_amount "PayPal net amount",
    r.rx_status "RX status",
    r.int_net_amount "JPMC net amount", r.int_currency "JPMC currency",
    r.int_profile "JPMC profile", r.int_date "JPMC date"
FROM summarize_rx(:'tx_date', :'tx_profile', :'tx_currency') r;

ROLLBACK;
