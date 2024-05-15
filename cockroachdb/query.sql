CREATE DATABASE IF NOT EXISTS learning;
SET database = learning;

BEGIN;

CREATE TABLE IF NOT EXISTS data_type (
  flg bool NOT NULL,
  num int NOT NULL,
  flt float NOT NULL,
  str string NOT NULL,
  dt date NOT NULL,
  tm time NOT NULL,
  ts timestamp NOT NULL,
  dur interval NOT NULL,
  arr int[] NOT NULL,
  bin bytes NOT NULL,
  obj jsonb NOT NULL
);

-- SHOW COLUMNS FROM data_type;
-- SHOW CREATE TABLE data_type;

INSERT INTO data_type(
  flg, num, flt, str, dt, tm, ts, dur, arr, bin, obj
) VALUES (
  true, 1, '+inf'::float, 'audi', '2024-05-11', '01:02:03.123456',
  '2024-05-11 01:02:03.123456+01:30', '1-2 3 04:05:06.123456',
  array[1, 2, 3], b'abc',
  '{"a": true, "b": 1, "c": "abc", "d": [null, false], "e": {"f": 1.2}}'
);

COMMIT;

-- DROP TABLE data_type;

-- SELECT dt.* FROM data_type dt;

-- SELECT (tpl).* FROM (SELECT (true, 1, 'a') tpl);
-- SELECT (tbl.*) FROM (SELECT true, 1, 'a') tbl;

-- WITH cte AS (SELECT true, 1, 'a')
-- SELECT cte.* FROM cte;
-- SELECT t.sin FROM sin(1.2) t(sin);
-- SELECT t.ser FROM generate_series(1, 3) t(ser);
-- SELECT t.* FROM (VALUES (10), (20), (30)) WITH ORDINALITY t(val, ord);
-- SELECT t.* FROM (VALUES (true, 1, 'a'), (false, 2, 'b')) t(a, b, c);

-- WITH RECURSIVE fact(n, val) AS (
--   SELECT 0, 1 -- initial subquery
--   UNION -- recursive subquery
--   SELECT n + 1, val * (n + 1) FROM fact WHERE n < 9
-- )
-- SELECT * FROM fact;

-- * Window function

-- SELECT DISTINCT(r.city),
--   sum(r.revenue) OVER () total_revenue,
--   sum(r.revenue) OVER (PARTITION BY r.city) city_revenue
-- FROM rides r;

-- SELECT DISTINCT(u.name) user_name,
--   count(*) OVER (PARTITION BY r.rider_id) user_rides
-- FROM rides r JOIN users u ON u.id = r.rider_id
-- ORDER BY user_rides DESC LIMIT 10;

-- SELECT DISTINCT(u.name) user_name,
--   sum(r.revenue) OVER (PARTITION BY r.rider_id) user_revenue
-- FROM rides r JOIN users u ON u.id = r.rider_id
-- ORDER BY user_revenue DESC LIMIT 10;

-- SELECT DISTINCT(t.key), count(*) OVER key, sum(t.val) OVER key
-- FROM (VALUES ('a', 1), ('a', 2), ('b', 3), ('b', 4), ('b', 5)) t(key, val)
-- WINDOW key AS (PARTITION BY t.key);
