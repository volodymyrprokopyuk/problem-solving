-- psql -h localhost -U postgres -d metrics -f database/tsdb.sql \
--   -Xq -v ON_ERROR_STOP=on -P linestyle=unicode

BEGIN; -- Node CPU utilization

DROP TABLE IF EXISTS node_cpu_long;

CREATE TABLE node_cpu_long (
  ts timestamptz NOT NULL,
  job text NOT NULL,
  instance text NOT NULL,
  cpu text NOT NULL,
  mode text NOT NULL,
  utilization double precision NULL,
  PRIMARY KEY (ts, job, instance, cpu, mode));

SELECT create_hypertable('node_cpu_long', 'ts');

DROP TABLE IF EXISTS node_cpu;

CREATE TABLE node_cpu (
  ts timestamptz NOT NULL,
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
  PRIMARY KEY (ts, job, instance, cpu));

SELECT create_hypertable('node_cpu', 'ts');

CREATE OR REPLACE FUNCTION transform_node_cpu()
RETURNS void
LANGUAGE SQL AS $$
INSERT INTO node_cpu
SELECT l.ts, l.job, l.instance, l.cpu,
  max(l.utilization) FILTER (WHERE l.mode = 'idle') idle,
  max(l.utilization) FILTER (WHERE l.mode = 'iowait') iowait,
  max(l.utilization) FILTER (WHERE l.mode = 'irq') irq,
  max(l.utilization) FILTER (WHERE l.mode = 'nice') nice,
  max(l.utilization) FILTER (WHERE l.mode = 'softirq') softirq,
  max(l.utilization) FILTER (WHERE l.mode = 'steal') steal,
  max(l.utilization) FILTER (WHERE l.mode = 'system') system,
  max(l.utilization) FILTER (WHERE l.mode = 'user') user
FROM node_cpu_long l
GROUP BY l.ts, l.job, l.instance, l.cpu;
$$;

CREATE OR REPLACE FUNCTION node_cpu(a_instance text)
RETURNS TABLE (
  tsb timestamptz[], idle double precision[], sysuser double precision[],
  iowait double precision[], other double precision[]
)
LANGUAGE SQL AS $$
WITH rate AS (
  SELECT time_bucket('30 sec', c.ts) tsb,
    rate(counter_agg(c.ts, c.idle)) idle,
    rate(counter_agg(c.ts, c.system + c.user)) sysuser,
    rate(counter_agg(c.ts, c.iowait)) iowait,
    rate(counter_agg(c.ts, c.irq + c.nice + c.softirq + c.steal)) other
  FROM node_cpu c WHERE c.instance ~* a_instance
  GROUP BY c.instance, c.cpu, tsb
  ORDER BY c.instance, c.cpu, tsb
), cpu AS (
  SELECT r.tsb, avg(r.idle) idle, avg(r.sysuser) sysuser,
    avg(r.iowait) iowait, avg(r.other) other
  FROM rate r
  GROUP BY r.tsb ORDER BY r.tsb
)
SELECT array_agg(c.tsb), array_agg(c.idle) idle, array_agg(c.sysuser) sysuser,
  array_agg(c.iowait) iowait, array_agg(c.other) other
FROM cpu c;
$$;

COMMIT;


BEGIN; -- Node memory consumption

DROP TABLE IF EXISTS node_memory;

CREATE TABLE node_memory (
  ts timestamptz NOT NULL,
  job text NOT NULL,
  instance text NOT NULL,
  free double precision NULL,
  PRIMARY KEY (ts, job, instance));

SELECT create_hypertable('node_memory', 'ts');

CREATE OR REPLACE FUNCTION node_memory(a_instance text)
RETURNS TABLE (tsb timestamptz[], used double precision[])
LANGUAGE SQL AS $$
WITH memory AS (
  SELECT time_bucket('15 sec', m.ts) tsb, avg(m.free) free
  FROM node_memory m WHERE m.instance ~* a_instance
  GROUP BY tsb ORDER BY tsb
)
SELECT array_agg(m.tsb) tsb,
  array_agg((16 * 2 ^ 30 - m.free) / (16 * 2 ^ 30) * 100) used
FROM memory m;
$$;

COMMIT;
