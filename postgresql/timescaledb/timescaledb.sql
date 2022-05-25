CREATE TEMPORARY TABLE metric (
  ts timestamp NOT NULL,
  val double precision NULL,
  PRIMARY KEY (ts, val)
);

INSERT INTO metric(ts, val) VALUES
  ('2022-05-24 00:00:01', 1),
  ('2022-05-24 00:00:16', 3),
  ('2022-05-24 00:00:31', 7),
  ('2022-05-24 00:00:46', 9),

  ('2022-05-24 00:01:01', 12),
  ('2022-05-24 00:01:16', 1),
  ('2022-05-24 00:01:31', 2),
  ('2022-05-24 00:01:46', 6),

  ('2022-05-24 00:02:01', 9),
  ('2022-05-24 00:02:16', 10),
  ('2022-05-24 00:02:31', 3),
  ('2022-05-24 00:02:46', 8);

-- SELECT time_bucket('1 min', m.ts) tsb, counter_agg(m.ts, m.val) cs
SELECT time_bucket('1 min', m.ts) tsb,
  delta(counter_agg(m.ts, m.val)),
  -- idelta_left(counter_agg(m.ts, m.val)),
  -- idelta_right(counter_agg(m.ts, m.val)),
  rate(counter_agg(m.ts, m.val)),
  -- irate_left(counter_agg(m.ts, m.val)),
  -- irate_right(counter_agg(m.ts, m.val)),
  time_delta(counter_agg(m.ts, m.val))
FROM metric m
GROUP BY tsb
ORDER BY tsb;



-- WITH metrics_wide AS (
--   SELECT time_bucket('1 minute', u.time) tbucket, u.job,
--     substring(u.instance FROM '^[^-]+') igroup,
--     avg(u.idle) idle, avg(u.system + u.user) system_user, avg(u.iowait) iowait
--   FROM node_cpu_utilization_wide u
--   WHERE u.instance ~* '^mongodb'
--   GROUP BY u.job, igroup, tbucket
--   ORDER BY u.job, igroup, tbucket)
-- SELECT array_agg(mw.tbucket) tbucket,
--   array_agg(mw.job) job,
--   array_agg(mw.igroup) igourp,
--   array_agg(mw.idle) idle,
--   array_agg(mw.iowait) iowait,
--   array_agg(mw.system_user) system_user
-- FROM metrics_wide mw;

-- BEGIN;

-- DROP TABLE IF EXISTS node_cpu_utilization_long;
-- DROP TABLE IF EXISTS node_cpu_utilization_wide;

-- CREATE TABLE node_cpu_utilization_long (
--   time timestamp NOT NULL,
--   job text NOT NULL,
--   instance text NOT NULL,
--   cpu text NOT NULL,
--   mode text NOT NULL,
--   utilization double precision NULL,
--   PRIMARY KEY (time, job, instance, cpu, mode));

-- SELECT create_hypertable('node_cpu_utilization_long', 'time');

-- CREATE TABLE node_cpu_utilization_wide (
--   time timestamp NOT NULL,
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
--   PRIMARY KEY (time, job, instance, cpu));

-- SELECT create_hypertable('node_cpu_utilization_wide', 'time');

-- CREATE OR REPLACE FUNCTION transform_node_cpu_utilization()
-- RETURNS void
-- LANGUAGE SQL AS $$
-- INSERT INTO node_cpu_utilization_wide
-- SELECT l.time, l.job, l.instance, l.cpu,
--   max(l.utilization) FILTER (WHERE l.mode = 'idle') idle,
--   max(l.utilization) FILTER (WHERE l.mode = 'iowait') iowait,
--   max(l.utilization) FILTER (WHERE l.mode = 'irq') irq,
--   max(l.utilization) FILTER (WHERE l.mode = 'nice') nice,
--   max(l.utilization) FILTER (WHERE l.mode = 'softirq') softirq,
--   max(l.utilization) FILTER (WHERE l.mode = 'steal') steal,
--   max(l.utilization) FILTER (WHERE l.mode = 'system') system,
--   max(l.utilization) FILTER (WHERE l.mode = 'user') "user"
-- FROM node_cpu_utilization_long l
-- GROUP BY l.time, l.job, l.instance, l.cpu;
-- $$;

-- COMMIT;
