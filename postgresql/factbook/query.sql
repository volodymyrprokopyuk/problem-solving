-- PREPARE factbook_by_period AS
-- SELECT fb.*
-- FROM factbook fb
-- WHERE date >= $1::date AND date < $1::date + $2::interval
-- ORDER BY date;
-- EXECUTE factbook_by_period('2017-02-01', '1 week')

/* PREPARE factbook_by_day AS */
/* SELECT dr.date::date, coalesce(shares, 0) shares, */
/*     coalesce(trades, 0) trades, coalesce(dollars, 0) dollars */
/* FROM generate_series($1::date, $1::date + $2::interval, '1 day'::interval) dr(date) */
/*     LEFT JOIN factbook fb ON fb.date = dr.date; */
/* EXECUTE factbook_by_day('2017-02-01', '1 month') */

/* PREPARE factbook_weekly_difference AS */
/* WITH two_last_weeks AS ( */
/*     SELECT dr.date::date, to_char(dr.date, 'Dy') "day", coalesce(fb.dollars, 0) dollars, */
/*         lag(fb.dollars, 1) OVER (PARTITION BY extract('isodow' FROM dr.date) */
/*             ORDER BY dr.date) last_week_dollars */
/*     FROM generate_series($1::date, $1::date + $2::interval, '1 day'::interval) dr(date) */
/*         LEFT JOIN factbook fb ON fb.date = dr.date) */
/* SELECT tlw.*, */
/*     CASE WHEN (tlw.dollars <> 0 AND tlw.last_week_dollars <> 0) */
/*         THEN round((tlw.dollars - tlw.last_week_dollars) / tlw.dollars::numeric, 4) */
/*         ELSE NULL */
/*     END difference */
/* FROM two_last_weeks tlw */
/* ORDER BY tlw.date; */
/* EXECUTE factbook_weekly_difference('2017-02-01', '1 month') */

SELECT jsonb_pretty(p.details) person_details
FROM (VALUES ('{"name": "Vlad", "profession": "architect"}'::jsonb),
    ('{"name": "Lana", "profession": "sales manager"}'::jsonb)) p(details)
WHERE p.details @> '{"name": "Vlad"}'
