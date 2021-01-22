-- SELECT f.flight_no, bp.seat_no, t.passenger_name, b.book_date
-- FROM flights f
--     JOIN boarding_passes bp ON bp.flight_id = f.flight_id
--     JOIN tickets t ON t.ticket_no = bp.ticket_no
--     JOIN bookings b ON b.book_ref = t.book_ref
-- WHERE f.departure_airport = 'SVO' AND f.arrival_airport = 'OVB'
--     AND f.actual_departure::date = bookings.now()::date - interval '2 days'
--     AND bp.seat_no = '1A'

-- SELECT f.flight_no, f.aircraft_code, bp.seat_no, count(*) OVER ()
-- FROM flights f
--     JOIN seats s ON s.aircraft_code = f.aircraft_code
--     LEFT JOIN boarding_passes bp
--         ON bp.flight_id = f.flight_id AND bp.seat_no = s.seat_no
-- WHERE f.actual_departure::date = bookings.now()::date - interval '1 day'
--     AND f.flight_no = 'PG0404' AND bp.seat_no IS NULL
-- LIMIT 1

-- WITH delayed_flights AS (
-- SELECT f.flight_no, (f.actual_departure - f.scheduled_departure) delay
-- FROM flights f
-- WHERE f.actual_departure IS NOT NULL AND f.scheduled_departure IS NOT NULL
--     AND f.actual_departure > f.scheduled_departure
-- )
-- SELECT df.*
-- FROM delayed_flights df
-- ORDER BY df.delay DESC
-- LIMIT 10

WITH ap AS (
SELECT dap.city departure_city, dap.airport_code departure_airport,
    aap.city arrival_city, aap.airport_code arrival_airport
FROM airports dap, airports aap
WHERE dap.city = 'Москва' AND aap.city = 'Санкт-Петербург'
)
SELECT DISTINCT f.flight_no, ap.departure_city, ap.departure_airport,
    ap.arrival_city, ap.arrival_airport,
    min(f.actual_arrival - f.actual_departure) OVER (PARTITION BY f.flight_no) min_duration,
    max(f.actual_arrival - f.actual_departure) OVER (PARTITION BY f.flight_no) max_duration,
    sum(CASE
        WHEN ((f.actual_departure - f.scheduled_departure) > interval '1 hour')
        THEN 1 ELSE 0 END) OVER (PARTITION BY f.flight_no) delayed_count,
    count(*) OVER (PARTITION BY f.flight_no) total_count
FROM ap
    JOIN flights f ON f.departure_airport = ap.departure_airport
        AND f.arrival_airport = ap.arrival_airport
WHERE f.actual_arrival IS NOT NULL
