-- psql -h localhost -U postgres -d metrics -f database/tsdb.sql \
--   -Xq -v ON_ERROR_STOP=on -P linestyle=unicode

-- BEGIN;

-- DROP TABLE IF EXISTS node_cpu_long;

-- CREATE TABLE node_cpu_long (
--   ts timestamptz NOT NULL,
--   job text NOT NULL,
--   instance text NOT NULL,
--   cpu text NOT NULL,
--   mode text NOT NULL,
--   utilization double precision NULL,
--   PRIMARY KEY (ts, job, instance, cpu, mode));

-- SELECT create_hypertable('node_cpu_long', 'ts');

-- DROP TABLE IF EXISTS node_cpu_wide;

-- CREATE TABLE node_cpu_wide (
--   ts timestamptz NOT NULL,
--   job text NOT NULL,
--   instance text NOT NULL,
--   cpu text NOT NULL,
--   idle double precision NULL,
--   iowait double precision NULL,
--   irq double precision NULL,
--   nice double precision NULL,
--   softirq double precision NULL,
--   steal double precision NULL,
--   system double precision NULL,
--   "user" double precision NULL,
--   PRIMARY KEY (ts, job, instance, cpu));

-- SELECT create_hypertable('node_cpu_wide', 'ts');

-- CREATE OR REPLACE FUNCTION transform_node_cpu()
-- RETURNS void
-- LANGUAGE SQL AS $$
-- INSERT INTO node_cpu_wide
-- SELECT l.ts, l.job, l.instance, l.cpu,
--   max(l.utilization) FILTER (WHERE l.mode = 'idle') idle,
--   max(l.utilization) FILTER (WHERE l.mode = 'iowait') iowait,
--   max(l.utilization) FILTER (WHERE l.mode = 'irq') irq,
--   max(l.utilization) FILTER (WHERE l.mode = 'nice') nice,
--   max(l.utilization) FILTER (WHERE l.mode = 'softirq') softirq,
--   max(l.utilization) FILTER (WHERE l.mode = 'steal') steal,
--   max(l.utilization) FILTER (WHERE l.mode = 'system') system,
--   max(l.utilization) FILTER (WHERE l.mode = 'user') user
-- FROM node_cpu_long l
-- GROUP BY l.ts, l.job, l.instance, l.cpu;
-- $$;

-- CREATE OR REPLACE FUNCTION node_cpu(a_instance text)
-- RETURNS TABLE (
--   tsb timestamptz[], job text[], instance text[],
--   idle double precision[], sysuser double precision[],
--   iowait double precision[], other double precision[]
-- )
-- LANGUAGE SQL AS $$
-- WITH counter AS (
--   SELECT time_bucket('1 min', c.ts) tsb, c.job,
--     substring(c.instance FROM '^[^-]+') instance,
--     counter_agg(c.ts, c.idle) idle,
--     counter_agg(c.ts, c.system + c.user) sysuser,
--     counter_agg(c.ts, c.iowait) iowait,
--     counter_agg(c.ts, c.irq + c.nice + c.softirq + c.steal) other
--   FROM node_cpu_wide c
--   WHERE c.instance ~* a_instance
--   GROUP BY c.job, instance, tsb
--   ORDER BY c.job, instance, tsb
-- ), rate AS (
--   SELECT c.tsb, c.job, c.instance,
--     -- extrapolated_rate(with_bounds(c.idle,
--     --   tstzrange(c.tsb, c.tsb + '1 min')), 'prometheus') idle,
--     -- extrapolated_rate(with_bounds(c.sysuser,
--     --   tstzrange(c.tsb, c.tsb + '1 min')), 'prometheus') sysuser,
--     -- extrapolated_rate(with_bounds(c.iowait,
--     --   tstzrange(c.tsb, c.tsb + '1 min')), 'prometheus') iowait,
--     -- extrapolated_rate(with_bounds(c.other,
--     --   tstzrange(c.tsb, c.tsb + '1 min')), 'prometheus') other

--     rate(c.idle) idle,
--     rate(c.sysuser) sysuser,
--     rate(c.iowait) iowait,
--     rate(c.other) other
--   FROM counter c
-- )
-- SELECT array_agg(r.tsb) tsb,
--   array_agg(r.job) job,
--   array_agg(r.instance) instance,
--   array_agg(r.idle) idle,
--   array_agg(r.sysuser) sysuser,
--   array_agg(r.iowait) iowait,
--   array_agg(r.other) other
-- FROM rate r;
-- $$;

-- COMMIT;


-- BEGIN;

-- DROP TABLE IF EXISTS node_memory;

-- CREATE TABLE node_memory (
--   ts timestamptz NOT NULL,
--   job text NOT NULL,
--   instance text NOT NULL,
--   free double precision NULL,
--   PRIMARY KEY (ts, job, instance));

-- SELECT create_hypertable('node_memory', 'ts');

CREATE OR REPLACE FUNCTION node_memory(a_instance text)
RETURNS TABLE (tsb timestamptz[], free double precision[])
LANGUAGE SQL AS $$
WITH memory AS (
  SELECT time_bucket('15 sec', m.ts) tsb, avg(m.free) free
  FROM node_memory m WHERE m.instance ~* a_instance
  GROUP BY tsb ORDER BY tsb
)
SELECT array_agg(m.tsb) tsb,
  array_agg(((16 * 2 ^ 30 - m.free) / 2 ^ 30) / 16 * 100) used
FROM memory m;
$$;

-- COMMIT;
