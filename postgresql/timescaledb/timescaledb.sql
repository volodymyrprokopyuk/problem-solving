-- SELECT count(*) FROM node_cpu_utilization_long;
-- SELECT count(*) FROM node_cpu_utilization_wide;

-- BEGIN;

-- DROP TABLE IF EXISTS node_cpu_utilization_long;
-- DROP TABLE IF EXISTS node_cpu_utilization_wide;

CREATE TABLE node_cpu_utilization_long (
  time timestamp NOT NULL,
  job text NOT NULL,
  instance text NOT NULL,
  cpu text NOT NULL,
  mode text NOT NULL,
  utilization double precision NULL,
  PRIMARY KEY (time, job, instance, cpu, mode));

SELECT create_hypertable('node_cpu_utilization_long', 'time');

CREATE TABLE node_cpu_utilization_wide (
  time timestamp NOT NULL,
  job text NOT NULL,
  instance text NOT NULL,
  cpu text NOT NULL,
  idle double precision NULL,
  iowait double precision NULL,
  irq double precision NULL,
  nice double precision NULL,
  softirq double precision NULL,
  steal double precision NULL,
  system double precision NULL,
  "user" double precision NULL,
  PRIMARY KEY (time, job, instance, cpu));

SELECT create_hypertable('node_cpu_utilization_wide', 'time');

CREATE OR REPLACE FUNCTION transform_node_cpu_utilization()
RETURNS void
LANGUAGE SQL AS $$
INSERT INTO node_cpu_utilization_wide
SELECT l.time, l.job, l.instance, l.cpu,
  max(l.utilization) FILTER (WHERE l.mode = 'idle') idle,
  max(l.utilization) FILTER (WHERE l.mode = 'iowait') iowait,
  max(l.utilization) FILTER (WHERE l.mode = 'irq') irq,
  max(l.utilization) FILTER (WHERE l.mode = 'nice') nice,
  max(l.utilization) FILTER (WHERE l.mode = 'softirq') softirq,
  max(l.utilization) FILTER (WHERE l.mode = 'steal') steal,
  max(l.utilization) FILTER (WHERE l.mode = 'system') system,
  max(l.utilization) FILTER (WHERE l.mode = 'user') "user"
FROM node_cpu_utilization_long l
GROUP BY l.time, l.job, l.instance, l.cpu;
$$;

-- COMMIT;


-- SELECT *
-- FROM stock_price sp
-- WHERE sp.symbol = 'AMZN' AND sp.day_volume IS NOT NULL
-- ORDER BY sp.time DESC, sp.day_volume
-- LIMIT 10;

-- SELECT avg(sp.price)
-- FROM stock_price sp
--   JOIN company c USING (symbol)
-- WHERE c.name = 'Apple' AND sp.time > now() - '4 days'::interval;

-- SELECT sp.symbol, first(sp.price, sp.time), last(sp.price, sp.time)
-- FROM stock_price sp
-- WHERE sp.time > now() - '4 days'::interval
--   AND sp.symbol IN ('AMZN', 'AAPL', 'MSFT', 'GOOG')
-- GROUP BY sp.symbol
-- ORDER BY sp.symbol;

-- SELECT time_bucket('1 day', time) bucket, sp.symbol, avg(sp.price)
-- FROM stock_price sp
-- WHERE sp.time > now() - '1 week'::interval
--   AND sp.symbol IN ('AMZN', 'AAPL', 'MSFT', 'GOOG')
-- GROUP BY sp.symbol, bucket
-- ORDER BY sp.symbol, bucket;


-- BEGIN;

-- CREATE TABLE stock_price (
--   time timestamptz NOT NULL,
--   symbol text NOT NULL,
--   price double precision NULL,
--   day_volume integer NULL);

-- -- Partition on time + index on time
-- SELECT create_hypertable('stock_price', 'time');

-- CREATE INDEX ix_stock_price_symbol_time ON stock_price (symbol, time DESC);

-- CREATE TABLE company (
--   symbol text NOT NULL,
--   name text NOT NULL);

-- \COPY stock_price FROM 'tutorial_sample_tick.csv' WITH (FORMAT csv, DELIMITER ',', HEADER);

-- \COPY company FROM 'tutorial_sample_company.csv' WITH (FORMAT csv, DELIMITER ',', HEADER);

-- COMMIT;


-- BEGIN;

-- CREATE TABLE conditions (
--   time timestamptz NOT NULL,
--   device integer NOT NULL,
--   temperature real NOT NULL,
--   PRIMARY KEY (time, device));

-- SELECT create_hypertable('conditions', 'time', 'device', 3);

-- INSERT INTO conditions(time, device, temperature)
-- SELECT t.ts, (random() * 4)::int, (random() * 80 - 40)::real
-- FROM generate_series(
--   '2022-04-01 00:00:00'::timestamptz, '2022-05-01 00:00:00'::timestamptz,
--   '5 mins'::interval) t(ts);

-- SELECT * FROM conditions c ORDER BY c.device, c.time LIMIT 10;

-- SELECT c.device, time_bucket('1 hour'::interval, c.time) bucket,
--   min(c.temperature), avg(c.temperature), max(c.temperature)
-- FROM conditions c
-- GROUP BY c.device, bucket
-- ORDER BY c.device, bucket
-- LIMIT 10;

-- ROLLBACK;
