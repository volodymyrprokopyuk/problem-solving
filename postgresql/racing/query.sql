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
