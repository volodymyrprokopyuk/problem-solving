-- SELECT d.code, d.forename, d.surname,
--     format('%s %s', d.forename, d.surname) full_name
-- FROM drivers d
-- ORDER BY full_name

-- SELECT dt.date, extract('isodow' FROM dt.date) dow, to_char(dt.date, 'dy') "day",
--     extract('isoyear' FROM dt.date) iso_year, extract('year' FROM dt.date) "year",
--     extract('week' FROM dt.date) week,
--     extract('day' FROM (dt.date + '2 months - 1 day'::interval)) feb,
--     extract('day' FROM (dt.date + '2 months - 1 day'::interval)) = 29 leap
-- FROM generate_series('2000-01-01'::date, '2010-01-01'::date, '1 year'::interval) dt(date)

-- SELECT d.code, d.forename, d.surname, count(*) win_count
-- FROM results r
--     JOIN drivers d USING (driverid)
-- WHERE r.position = 1
-- GROUP BY d.driverid
-- ORDER BY win_count DESC
-- LIMIT 10

-- SELECT rc.name race_name, rc.date race_date, d.surname winner
-- FROM results rs
--     JOIN races rc USING (raceid)
--     JOIN drivers d USING (driverid)
-- WHERE rs.position = 1
--     AND rc.date <@ daterange('2017-04-01'::date,
--         ('2017-04-01'::date + '3 months'::interval)::date)
--     -- AND rc.date >= '2017-04-01'::date
--     -- AND rc.date < '2017-04-01'::date + '3 months'::interval

-- SELECT d.surname, d.forename, count(*) failure_count
-- FROM results rs
--     JOIN drivers d USING (driverid)
--     JOIN races rc USING (raceid)
-- WHERE rc.date <@ daterange('1978-01-01'::date,
--     ('1978-01-01'::date + '1 year'::interval)::date)
--     AND NOT EXISTS (
--         SELECT 1
--         FROM results r
--         WHERE r.driverid = d.driverid AND r.resultid = rs.resultid
--             AND r.position IS NOT NULL)
-- GROUP BY d.driverid
-- ORDER BY failure_count DESC

-- SELECT d.code, d.surname, rs.position, rs.laps, s.status
-- FROM results rs
--     JOIN drivers d USING (driverid)
--     JOIN status s USING (statusid)
-- WHERE rs.raceid = 972
-- ORDER BY rs.position NULLS LAST, rs.laps DESC,
--     CASE s.status WHEN 'Power Unit' THEN 1 ELSE 2 END

-- SELECT c.name, c.location, c.country
-- FROM circuits c
-- ORDER BY point(c.lng, c.lat) <-> point(2.349014, 48.864716)
-- LIMIT 10

-- BEGIN;
-- ALTER TABLE circuits ADD COLUMN position point;
-- UPDATE circuits SET position = point(lng, lat);
-- CREATE INDEX ON circuits USING gist(position);
-- COMMIT;

-- SELECT c.name, c.location, c.country
-- FROM circuits c
-- ORDER BY c.position <-> point(2.349014, 48.864716)
-- LIMIT 10

-- WITH race_decade AS (
--     SELECT DISTINCT extract('decade' FROM rc.date) decade
--     FROM races rc)
-- SELECT tw.decade * 10 decade, tw.surname, tw.win_count,
--     row_number() OVER dwc AS position
-- FROM race_decade rd
--     JOIN LATERAL (
--         SELECT extract('decade' FROM rc.date) decade, d.surname, count(*) win_count
--         FROM results rs
--             JOIN races rc USING (raceid)
--             JOIN drivers d USING (driverid)
--         WHERE extract('decade' FROM rc.date) = rd.decade AND rs.position = 1
--         GROUP BY decade, d.driverid
--         ORDER BY win_count DESC
--         LIMIT 3) tw ON true -- top winner
-- WINDOW dwc AS (PARTITION BY tw.decade ORDER BY tw.win_count DESC)
-- ORDER BY tw.decade, win_count DESC

-- WITH decade_winner AS (
--     SELECT DISTINCT extract('decade' FROM rc.date) decade, d.surname,
--         count(*) OVER dd win_count
--     FROM results rs
--         JOIN races rc USING (raceid)
--         JOIN drivers d USING (driverid)
--     WHERE rs.position = 1
--     WINDOW dd AS (PARTITION BY extract('decade' FROM rc.date), d.driverid)),
-- winner_position AS (
--     SELECT dw.decade, dw.surname, dw.win_count, row_number() OVER dwc AS position
--     FROM decade_winner dw
--     WINDOW dwc AS (PARTITION BY dw.decade ORDER BY dw.win_count DESC))
-- SELECT wp.decade * 10 decade, wp.surname, wp.win_count, wp.position
-- FROM winner_position wp
-- WHERE wp.position <= 3

-- SELECT l.lap, d.surname, l.position, l.time
-- FROM laptimes l
--     JOIN drivers d USING (driverid)
-- WHERE l.raceid = 972 AND ROW(l.lap, l.position) > (1, 3)
-- ORDER BY l.lap, l.position
-- FETCH FIRST 3 ROWS ONLY

-- WITH decade_race AS (
--     SELECT extract('decade' FROM rc.date) * 10 decade, count(*) race_count
--     FROM races rc
--     GROUP BY decade
--     ORDER BY decade)
-- SELECT dr.decade, dr.race_count,
--     dr.race_count - lag(dr.race_count, 1) OVER (ORDER BY dr.decade) race_delta
-- FROM decade_race dr

-- SELECT d.surname, bool_and(rs.position IS NULL) never_finished
-- FROM results rs
--     JOIN drivers d USING (driverid)
-- GROUP BY d.driverid
-- HAVING bool_and(rs.position IS NULL)

-- WITH race_stat AS (
--     SELECT DISTINCT d.surname, count(*) OVER d race_total,
--         count(rs.position IS NULL) OVER d failed_count,
--         bool_and(rs.position IS NULL) OVER d never_finished
--     FROM results rs
--         JOIN drivers d USING (driverid)
--     WINDOW d AS (PARTITION BY d.driverid))
-- SELECT rs.surname, rs.race_total, rs.failed_count
-- FROM race_stat rs
-- -- WHERE rs.failed_count = rs.race_total AND rs.race_total = 1
-- WHERE rs.never_finished
-- ORDER BY rs.failed_count DESC

-- WITH year_race AS (
--     SELECT extract('year' FROM rc.date) "year",
--         count(*) FILTER (WHERE rs.position IS NULL) failed_count
--     FROM results rs
--         JOIN races rc USING (raceid)
--     GROUP BY extract('year' FROM rc.date))
-- SELECT yr.year, yr.failed_count
-- FROM year_race yr
-- ORDER BY yr.failed_count DESC

-- SELECT DISTINCT extract('year' FROM rc.date) "year",
--     count(*) FILTER (WHERE rs.position IS NULL) OVER y failed_count
-- FROM results rs
--     JOIN races rc USING (raceid)
-- WINDOW y AS (PARTITION BY extract('year' FROM rc.date))
-- ORDER BY failed_count DESC

-- SELECT s.status, count(*) status_count
-- FROM results rs
--     JOIN races rc USING (raceid)
--     JOIN status s USING (statusid)
-- WHERE rs.position IS NULL
--     AND rc.date <@ '[1978-01-01, 1979-01-01)'::daterange
-- GROUP BY s.status
-- HAVING count(*) >= 10
-- ORDER BY status_count DESC

-- SELECT d.surname driver, c.name constructor, sum(rs.points) points
-- FROM results rs
--     JOIN races rc USING (raceid)
--     JOIN drivers d USING (driverid)
--     JOIN constructors c USING (constructorid)
-- WHERE rc.date <@ '[1978-01-01, 1979-01-01)'::daterange
-- GROUP BY GROUPING SETS ((driver), (constructor))
-- ORDER BY d.surname IS NOT NULL, points DESC

-- SELECT d.surname driver, c.name constructor, sum(rs.points) points
-- FROM results rs
--     JOIN races rc USING (raceid)
--     JOIN drivers d USING (driverid)
--     JOIN constructors c USING (constructorid)
-- WHERE d.surname IN ('Prost', 'Senna')
-- -- GROUP BY ROLLUP (driver, constructor)
-- GROUP BY CUBE (driver, constructor)
-- ORDER BY driver, points DESC

-- SELECT p.brand, p.size, sum(p.price) price
-- FROM (VALUES
--     ('Marmot', 'M', 10), ('Marmot', 'L', 20),
--     ('North Face', 'M', 15), ('North Face', 'L', 25))
--     p(brand, size, price)
-- -- GROUP BY GROUPING SETS ((p.brand), (p.size), ())
-- -- GROUP BY ROLLUP (p.brand, p.size)
-- GROUP BY CUBE (p.brand, p.size)
-- ORDER BY p.brand, price DESC
