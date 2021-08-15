\set ECHO all

\set analyze 0

\if :analyze
\set explain 'EXPLAIN (COSTS OFF)'
\else
\set explain
\endif

BEGIN;

CREATE SCHEMA ods;

SET search_path TO ods;

-- DATA SCHEMA

CREATE TABLE ods.pstep_type (
  pstep_type text PRIMARY KEY);

CREATE TABLE ods.pstep (
  pstep_id text PRIMARY KEY,
  pstep_type text NOT NULL REFERENCES pstep_type (pstep_type),
  pstep_message jsonb NOT NULL);

CREATE INDEX ON pstep USING GIN (pstep_message jsonb_path_ops);

CREATE TABLE ods.plink (
  from_pstep_id text NOT NULL REFERENCES pstep (pstep_id),
  to_pstep_id text NOT NULL REFERENCES pstep (pstep_id),
  UNIQUE (from_pstep_id, to_pstep_id));

CREATE INDEX ON plink (to_pstep_id);

-- DATA FUNCTIONS

CREATE FUNCTION ods.random_string(length integer)
RETURNS text LANGUAGE SQL AS $$
  SELECT string_agg(substring(
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',
    (random() * 62 + 1)::integer, 1), '')
  FROM generate_series(1, length);
$$;

CREATE FUNCTION ods.random_pstep_type()
RETURNS text LANGUAGE SQL AS $$
  SELECT st.pstep_type FROM pstep_type st ORDER BY random() LIMIT 1;
$$;

-- DATA LOAD

\set ECHO errors

INSERT INTO pstep_type(pstep_type)
VALUES
  ('pain.001.gheader'), ('pain.001.pinfo'), ('pain.001.tinfo'),
  ('pain.002.gheader'), ('pain.002.pstatus'), ('pain.002.tstatus');

\if :analyze
INSERT INTO pstep (pstep_id, pstep_type, pstep_message)
SELECT random_string(10), random_pstep_type(), '{}'
FROM generate_series(1, 9e4);

WITH numbered_pstep AS (
    SELECT s.pstep_id, row_number() OVER () FROM pstep s)
INSERT INTO plink (from_pstep_id, to_pstep_id)
SELECT sf.pstep_id, st.pstep_id
  FROM numbered_pstep sf
    JOIN numbered_pstep st ON sf.row_number = st.row_number + 1;
\endif

INSERT INTO pstep (pstep_id, pstep_type, pstep_message)
VALUES
  -- pain.001 - Customer credit transfer initiation
  ('INIT100', 'pain.001.gheader',
   '{"creation_date": "2021-08-10"}'),
  ('PAY110', 'pain.001.pinfo',
   '{"debtor_account": "IBAN001", "execution_date": "2021-08-10"}'),
  ('TX111', 'pain.001.tinfo',
   '{"amount": 111.00, "currency": "EUR"}'),
  ('TX112', 'pain.001.tinfo',
   '{"amount": 112.00, "currency": "EUR"}'),
  ('PAY120', 'pain.001.pinfo',
   '{"debtor_account": "IBAN002", "execution_date": "2021-08-11"}'),
  ('TX121', 'pain.001.tinfo',
   '{"amount": 121.00, "currency": "GBP"}'),

  ('INIT200', 'pain.001.gheader',
   '{"creation_date": "2021-08-20"}'),
  ('PAY210', 'pain.001.pinfo',
   '{"debtor_account": "IBAN003", "execution_date": "2021-08-20"}'),
  ('TX211', 'pain.001.tinfo',
   '{"amount": 211.00, "currency": "GBP"}'),
  ('TX212', 'pain.001.tinfo',
   '{"amount": 212.00, "currency": "GBP"}'),
  ('PAY220', 'pain.001.pinfo',
   '{"debtor_account": "IBAN001", "execution_date": "2021-08-21"}'),
  ('TX221', 'pain.001.tinfo',
   '{"amount": 221.00, "currency": "EUR"}'),
  -- pain.002 - Customer payment status report
  ('REP100', 'pain.002.gheader', '{}'),
  ('RPAY110', 'pain.002.pstatus',
   '{"pstep_id": "PAY110", "payment_status": "ACC+PEND",
     "confirmation_date": "2021-08-10"}'),
  ('RTX111', 'pain.002.tstatus',
   '{"pstep_id": "TX111", "tx_status": "ACCEPTED",
     "confirmation_date": "2021-08-10"}'),
  ('RTX112', 'pain.002.tstatus',
   '{"pstep_id": "TX112", "tx_status": "PENDING",
     "confirmation_date": "2021-08-10"}'),
  ('RPAY120', 'pain.002.pstatus',
   '{"pstep_id": "PAY120", "payment_status": "REJECTED",
     "confirmation_date": "2021-08-11"}'),
  ('RTX121', 'pain.002.tstatus',
   '{"pstep_id": "TX121", "tx_status": "REJECTED",
     "confirmation_date": "2021-08-11"}'),

  ('REP101', 'pain.002.gheader', '{}'),
  ('RPAY111', 'pain.002.pstatus',
   '{"pstep_id": "PAY110", "payment_status": "ACCEPTED",
     "confirmation_date": "2021-08-12"}'),
  ('RTX113', 'pain.002.tstatus',
   '{"pstep_id": "TX112", "tx_status": "ACCEPTED",
     "confirmation_date": "2021-08-12"}'),

  ('REP200', 'pain.002.gheader', '{}'),
  ('RPAY210', 'pain.002.pstatus',
   '{"pstep_id": "PAY210", "payment_status": "ACC+REJ",
     "confirmation_date": "2021-08-20"}'),
  ('RTX211', 'pain.002.tstatus',
   '{"pstep_id": "TX211", "tx_status": "ACCEPTED",
     "confirmation_date": "2021-08-20"}'),
  ('RTX212', 'pain.002.tstatus',
   '{"pstep_id": "TX212", "tx_status": "REJECTED",
     "confirmation_date": "2021-08-20"}'),
  ('RPAY220', 'pain.002.pstatus',
   '{"pstep_id": "PAY220", "payment_status": "PENDING",
     "confirmation_date": "2021-08-21"}'),
  ('RTX221', 'pain.002.tstatus',
   '{"pstep_id": "TX221", "tx_status": "PENDING",
     "confirmation_date": "2021-08-21"}'),

  ('REP201', 'pain.002.gheader', '{}'),
  ('RPAY221', 'pain.002.pstatus',
   '{"pstep_id": "PAY220", "payment_status": "ACCEPTED",
     "confirmation_date": "2021-08-22"}'),
  ('RTX222', 'pain.002.tstatus',
   '{"pstep_id": "TX221", "tx_status": "ACCEPTED",
     "confirmation_date": "2021-08-22"}');

INSERT INTO plink (from_pstep_id, to_pstep_id)
VALUES
  -- pain.001 - Customer credit transfer initiation
  ('PAY110', 'INIT100'), ('TX111', 'PAY110'), ('TX112', 'PAY110'),
  ('PAY120', 'INIT100'), ('TX121', 'PAY120'),

  ('PAY210', 'INIT200'), ('TX211', 'PAY210'), ('TX212', 'PAY210'),
  ('PAY220', 'INIT200'), ('TX221', 'PAY220'),
  -- pain.002 - Customer payment status report
  ('RPAY110', 'REP100'), ('RTX111', 'RPAY110'), ('RTX112', 'RPAY110'),
  ('RPAY120', 'REP100'), ('RTX121', 'RPAY120'),

  ('RPAY111', 'REP101'), ('RTX113', 'RPAY111'),

  ('RPAY210', 'REP200'), ('RTX211', 'RPAY210'), ('RTX212', 'RPAY210'),
  ('RPAY220', 'REP200'), ('RTX221', 'RPAY220'),

  ('RPAY221', 'REP201'), ('RTX222', 'RPAY221');

\set ECHO all

-- DATA QUERING

ANALYZE;

-- Get a payment step by id

\set pstep_id 'INIT100'
:explain
SELECT s.* FROM pstep s WHERE s.pstep_id = :'pstep_id';

-- Get all upstream payment steps linked to a payment step id

\set pstep_id 'TX111'
\set depth 2
:explain
WITH RECURSIVE linked_pstep AS (
  SELECT s.pstep_id, 0 depth FROM pstep s WHERE s.pstep_id = :'pstep_id'
  UNION ALL
  SELECT s.pstep_id, ls.depth + 1 depth
  FROM linked_pstep ls
    JOIN plink l ON l.from_pstep_id = ls.pstep_id
    JOIN pstep s ON s.pstep_id = l.to_pstep_id
  WHERE ls.depth < :depth)
SELECT s.* FROM linked_pstep ls JOIN pstep s USING (pstep_id);

-- Get all downstream payment steps linked to a payment step id

\set pstep_id 'INIT100'
\set depth 2
:explain
WITH RECURSIVE linked_pstep AS (
  SELECT s.pstep_id, 0 depth FROM pstep s WHERE s.pstep_id = :'pstep_id'
  UNION ALL
  SELECT s.pstep_id, ls.depth + 1 depth
  FROM linked_pstep ls
    JOIN plink l ON l.to_pstep_id = ls.pstep_id
    JOIN pstep s ON s.pstep_id = l.from_pstep_id
  WHERE ls.depth < :depth)
SELECT s.* FROM linked_pstep ls JOIN pstep s USING (pstep_id);

-- Get all payment steps for a debtor account

\set debtor_account 'IBAN001'
:explain
SELECT s.*
FROM pstep s
WHERE s.pstep_message @@
  format('$.debtor_account == "%s"', :'debtor_account')::jsonpath;

-- Get all payment steps for a date range

\set from_date 2021-08-10
\set to_date 2021-08-11
:explain
SELECT s.*
FROM pstep s
WHERE s.pstep_message @@
  format('($.execution_date.datetime() >= "%s".datetime() &&
    $.execution_date.datetime() <= "%s".datetime()) ||
    ($.creation_date.datetime() >= "%s".datetime() &&
    $.creation_date.datetime() <= "%s".datetime())',
    :'from_date', :'to_date', :'from_date', :'to_date')::jsonpath;

-- Get all payment steps for an amount range and currency

\set from_amount 100.00
\set to_amount 200.00
\set currency EUR
:explain
SELECT s.*
FROM pstep s
WHERE s.pstep_message @@
  format('$.amount >= %s && $.amount <= %s && $.currency == "%s"',
    :from_amount, :to_amount, :'currency')::jsonpath;

-- Get status changes of a payment step by id

\set pstep_id PAY110
:explain
SELECT s.*
FROM pstep s
WHERE s.pstep_message @@
  format('$.pstep_id == "%s"', :'pstep_id')::jsonpath
ORDER BY (s.pstep_message->>'confirmation_date')::date;

ROLLBACK;
