\set ECHO all

BEGIN;

CREATE SCHEMA ods;

SET search_path TO ods;

-- UTILS

CREATE FUNCTION to_date(d text)
RETURNS date LANGUAGE sql IMMUTABLE AS $$ SELECT d::date; $$;

-- DATA SCHEMA

-- pain.001 - Customer credit transfer initiation

CREATE TABLE ods.cct_initiation (
  initiation_id text PRIMARY KEY,
  initiation_message jsonb NOT NULL);

CREATE TABLE ods.cct_payment (
  payment_id text PRIMARY KEY,
  payment_message jsonb NOT NULL,
  initiation_id text NOT NULL REFERENCES cct_initiation(initiation_id));

CREATE INDEX ON cct_payment (initiation_id);
CREATE INDEX ON cct_payment (to_date(payment_message->>'execution_date'));
CREATE INDEX ON cct_payment USING GIN (payment_message jsonb_path_ops);

CREATE TABLE ods.cct_transaction (
  transaction_id text PRIMARY KEY,
  transaction_message jsonb NOT NULL,
  payment_id text NOT NULL REFERENCES cct_payment(payment_id));

CREATE INDEX ON cct_transaction (payment_id);
CREATE INDEX ON cct_transaction (((transaction_message->'amount')::numeric));

-- pain.002 - Customer payment status report

CREATE TABLE ods.cps_report (
  report_id text PRIMARY KEY,
  report_message jsonb NOT NULL,
  original_initiation_id text NOT NULL, -- REFERENCES cct_initiation(initiation_id)
  original_initiation_message jsonb NOT NULL);

CREATE INDEX ON cps_report (original_initiation_id);

CREATE TABLE ods.cps_payment (
  payment_id text PRIMARY KEY,
  payment_message jsonb NOT NULL,
  original_payment_id text NOT NULL, -- REFERENCES cct_payment(payment_id)
  original_payment_message jsonb NOT NULL,
  report_id text NOT NULL REFERENCES cps_report(report_id));

CREATE INDEX ON cps_payment (original_payment_id);
CREATE INDEX ON cps_payment (report_id);

CREATE TABLE ods.cps_transaction (
  transaction_id text PRIMARY KEY,
  transaction_message jsonb NOT NULL,
  original_transaction_id text NOT NULL, -- REFERENCES cct_transaction(transaction_id)
  original_transaction_message jsonb NOT NULL,
  payment_id text NOT NULL REFERENCES cps_payment(payment_id));

CREATE INDEX ON cps_transaction (original_transaction_id);
CREATE INDEX ON cps_transaction (payment_id);

-- DATA LOAD

\set ECHO errors

-- pain.001 - Customer credit transfer initiation

INSERT INTO cct_initiation(initiation_id, initiation_message)
VALUES
  ('INIT100', '{"initiation_id": "INIT100"}'),
  ('INIT200', '{"initiation_id": "INIT200"}');

TABLE cct_initiation;

INSERT INTO cct_payment(payment_id, payment_message, initiation_id)
VALUES
  ('PAY110',
   '{"payment_id": "PAY110", "execution_date": "2021-08-09", "debtor": "Debtor A"}',
   'INIT100'),
  ('PAY120',
   '{"payment_id": "PAY120", "execution_date": "2021-08-10", "debtor": "Debtor B"}',
   'INIT100'),
  ('PAY210',
   '{"payment_id": "PAY210", "execution_date": "2021-08-11", "debtor": "Debtor A"}',
   'INIT200'),
  ('PAY220',
   '{"payment_id": "PAY220", "execution_date": "2021-08-12", "debtor": "Debtor B"}',
   'INIT200');

TABLE cct_payment;

INSERT INTO cct_transaction(transaction_id, transaction_message, payment_id)
VALUES
  ('TX111','{"transaction_id": "TX111", "amount": 110.00}', 'PAY110'),
  ('TX112','{"transaction_id": "TX112", "amount": 111.00}', 'PAY110'),
  ('TX121','{"transaction_id": "TX121", "amount": 120.00}', 'PAY120'),
  ('TX211','{"transaction_id": "TX211", "amount": 210.00}', 'PAY210'),
  ('TX212','{"transaction_id": "TX212", "amount": 211.00}', 'PAY210'),
  ('TX221','{"transaction_id": "TX221", "amount": 220.00}', 'PAY220');

TABLE cct_transaction;

-- pain.002 - Customer payment status report

INSERT INTO cps_report(report_id, report_message,
  original_initiation_id, original_initiation_message)
VALUES
  ('REP100', '{"report_id": "REP100"}', 'INIT100', '{"initiation_id": "INIT100"}'),
  ('REP101', '{"report_id": "REP101"}', 'INIT100', '{"initiation_id": "INIT100"}'),
  ('REP200', '{"report_id": "REP200"}', 'INIT200', '{"initiation_id": "INIT200"}');

TABLE cps_report;

INSERT INTO cps_payment(payment_id, payment_message,
  original_payment_id, original_payment_message, report_id)
VALUES
  ('RPAY110', '{"payment_id": "RPAY110", "payment_status": "ACC+REJ"}',
   'PAY110', '{"payment_id": "PAY110"}', 'REP100'),
  ('RPAY111', '{"payment_id": "RPAY111", "payment_status": "ACCEPTED"}',
   'PAY110', '{"payment_id": "PAY110"}', 'REP101'),
  ('RPAY120', '{"payment_id": "RPAY120", "payment_status": "ACCEPTED"}',
   'PAY120', '{"payment_id": "PAY120"}', 'REP100'),
  ('RPAY210', '{"payment_id": "RPAY210", "payment_status": "ACC+PEND"}',
   'PAY210', '{"payment_id": "PAY210"}', 'REP200'),
  ('RPAY220', '{"payment_id": "RPAY220", "payment_status": "ACCEPTED"}',
   'PAY220', '{"payment_id": "PAY220"}', 'REP200');

TABLE cps_payment;

INSERT INTO cps_transaction(transaction_id, transaction_message,
  original_transaction_id, original_transaction_message, payment_id)
VALUES
  ('RTX111',
   '{"transaction_id": "RTX111", "transaction_status": "ACCEPTED",
     "confirmed_date": "2021-08-19"}',
   'TX111', '{"transaction_id": "TX111"}', 'RPAY110'),
  ('RTX112',
   '{"transaction_id": "RTX112", "transaction_status": "REJECTED",
     "confirmed_date": "2021-08-19"}',
   'TX112', '{"transaction_id": "TX112"}', 'RPAY110'),
  ('RTX113',
   '{"transaction_id": "RTX112", "transaction_status": "ACCEPTED",
     "confirmed_date": "2021-08-29"}',
   'TX112', '{"transaction_id": "TX112"}', 'RPAY111'),
  ('RTX121',
   '{"transaction_id": "RTX121", "transaction_status": "ACCEPTED",
     "confirmed_date": "2021-08-19"}',
   'TX121', '{"transaction_id": "TX121"}', 'RPAY120'),
  ('RTX211',
   '{"transaction_id": "RTX211", "transaction_status": "ACCEPTED",
     "confirmed_date": "2021-08-19"}',
   'TX211', '{"transaction_id": "TX211"}', 'RPAY210'),
  ('RTX212',
   '{"transaction_id": "RTX212", "transaction_status": "PENDING",
     "confirmed_date": "2021-08-19"}',
   'TX212', '{"transaction_id": "TX212"}', 'RPAY210'),
  ('RTX221',
   '{"transaction_id": "RTX221", "transaction_status": "ACCEPTED",
     "confirmed_date": "2021-08-19"}',
   'TX221', '{"transaction_id": "TX221"}', 'RPAY220');

TABLE cps_transaction;

\set ECHO all

-- DATA QUERYING

\set explain
-- \set explain 'EXPLAIN (COSTS OFF)'

-- Get message by id

:explain
SELECT i.initiation_message
FROM cct_initiation i
WHERE i.initiation_id = 'INIT100';

:explain
SELECT r.report_message
FROM cps_report r
WHERE r.report_id = 'REP100';

-- Get payment by id

:explain
SELECT p.payment_message
FROM cct_payment p
WHERE p.payment_id = 'PAY110';

:explain
SELECT p.payment_message payment_report
FROM cps_payment p
WHERE p.payment_id = 'RPAY110';

-- Get transaction by id

:explain
SELECT t.transaction_message
FROM cct_transaction t
WHERE t.transaction_id = 'TX111';

:explain
SELECT t.transaction_message transaction_report
FROM cps_transaction t
WHERE t.transaction_id = 'RTX111';

-- Get all payments (information + status) for a message id

:explain
SELECT i.initiation_message, p.payment_message,
  ps.payment_message payment_status
FROM cct_initiation i
  JOIN cct_payment p USING (initiation_id)
  LEFT JOIN cps_payment ps
    ON ps.original_payment_id = p.payment_id
WHERE i.initiation_id = 'INIT100';

-- Get all transactions (information + status) for a payment id
-- Get end-to-end payment life cycle

:explain
WITH payment_life_cycle AS (
    SELECT p.payment_message, t.transaction_message,
    ts.transaction_message transaction_status,
    row_number() OVER (
        PARTITiON BY t.transaction_id
        ORDER BY (ts.transaction_message->>'confirmed_date')::date) tx_position
    FROM cct_payment p
    JOIN cct_transaction t USING (payment_id)
    LEFT JOIN cps_transaction ts
        ON ts.original_transaction_id = t.transaction_id
    WHERE p.payment_id = 'PAY110')
SELECT p.payment_message, p.transaction_message, p.transaction_status
FROM payment_life_cycle p
ORDER BY p.tx_position;

-- Get the latest status of each transaction for a payment id

:explain
SELECT DISTINCT p.payment_message, t.transaction_message,
    last_value(ts.transaction_message) OVER (
        PARTITiON BY t.transaction_id
        ORDER BY (ts.transaction_message->>'confirmed_date')::date
        RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) transaction_status
FROM cct_payment p
JOIN cct_transaction t USING (payment_id)
LEFT JOIN cps_transaction ts
    ON ts.original_transaction_id = t.transaction_id
WHERE p.payment_id = 'PAY110'; --)

-- Get all transactions (+ corresponding payments) for an amount range

:explain
SELECT t.transaction_message, p.payment_message
FROM cct_transaction t
  JOIN cct_payment p USING (payment_id)
WHERE (t.transaction_message->'amount')::numeric >= 100.00
  AND (t.transaction_message->'amount')::numeric <= 200.00;
-- WHERE numrange(100.00, 200.00, '[]') @> (t.transaction_message->'amount')::numeric;
-- WHERE t.transaction_message @@
--   format('$.amount >= %s && $.amount <= %s', 100.00, 200.00)::jsonpath;

-- Get all payments (+ corresponding transactions) for a date range

:explain
SELECT p.payment_message, t.transaction_message
FROM cct_payment p
  JOIN cct_transaction t USING (payment_id)
WHERE to_date(p.payment_message->>'execution_date') >= '2021-08-10'
  AND to_date(p.payment_message->>'execution_date') <= '2021-08-11';
-- WHERE daterange('2021-08-10', '2021-08-11', '[]')
--   @> to_date(p.payment_message->>'execution_date');
-- WHERE p.payment_message @@
--   format('$.execution_date.datetime() >= "%s".datetime() &&
--     $.execution_date.datetime() <= "%s".datetime()',
--     '2021-08-10', '2021-08-11')::jsonpath;

-- Get all payments (+ corresponding transactions) for a debtor

:explain
SELECT p.payment_message, t.transaction_message
FROM cct_payment p
  JOIN cct_transaction t USING (payment_id)
WHERE p.payment_message @@ format('$.debtor == "%s"', 'Debtor A')::jsonpath;

ROLLBACK;
