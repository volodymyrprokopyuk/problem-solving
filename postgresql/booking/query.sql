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

-- WITH ap AS (
-- SELECT dap.city departure_city, dap.airport_code departure_airport,
--     aap.city arrival_city, aap.airport_code arrival_airport
-- FROM airports dap, airports aap
-- WHERE dap.city = 'Москва' AND aap.city = 'Санкт-Петербург'
-- )
-- SELECT DISTINCT f.flight_no, ap.departure_city, ap.departure_airport,
--     ap.arrival_city, ap.arrival_airport,
--     min(f.actual_arrival - f.actual_departure) OVER (PARTITION BY f.flight_no) min_duration,
--     max(f.actual_arrival - f.actual_departure) OVER (PARTITION BY f.flight_no) max_duration,
--     sum(CASE
--         WHEN ((f.actual_departure - f.scheduled_departure) > interval '1 hour')
--         THEN 1 ELSE 0 END) OVER (PARTITION BY f.flight_no) delayed_count,
--     count(*) OVER (PARTITION BY f.flight_no) total_count
-- FROM ap
--     JOIN flights f ON f.departure_airport = ap.departure_airport
--         AND f.arrival_airport = ap.arrival_airport
-- WHERE f.actual_arrival IS NOT NULL

-- WITH first_registration AS (
-- SELECT t.passenger_name, t.ticket_no,
--     count(*) OVER (PARTITION BY t.passenger_name, t.ticket_no) registration_count
-- FROM tickets t
--     JOIN boarding_passes bp ON bp.ticket_no = t.ticket_no
-- WHERE bp.boarding_no = 1
-- )
-- SELECT fr.*
-- FROM first_registration fr
-- WHERE fr.registration_count > 1

-- WITH booking_ticket AS (
-- SELECT count(*) ticket_count
-- FROM tickets t
-- GROUP BY t.book_ref
-- )
-- SELECT min(ticket_count) min_ticket, max(ticket_count) max_ticket
-- FROM booking_ticket bt

-- WITH booking_ticket AS (
-- SELECT t.book_ref, count(*) OVER (PARTITION BY t.book_ref) ticket_count
-- FROM tickets t
-- )
-- SELECT DISTINCT bt.ticket_count,
--     count(*) OVER (PARTITION BY bt.ticket_count) booking_count
-- FROM booking_ticket bt
-- ORDER BY bt.ticket_count

-- SELECT t.ticket_no, f.departure_airport origin, f.arrival_airport destination,
--     f.scheduled_departure depature, f.scheduled_arrival arrival,
--     f.scheduled_departure - lag(f.scheduled_arrival)
--         OVER (PARTITION BY t.ticket_no ORDER BY f.scheduled_departure) time_between
-- FROM tickets t
--     JOIN ticket_flights tf ON tf.ticket_no = t.ticket_no
--     JOIN flights f ON f.flight_id = tf.flight_id
--     JOIN bookings b ON b.book_ref = t.book_ref
-- WHERE b.book_date::date = bookings.now()::date - interval '1 week'
--     AND t.ticket_no = '0005434082600'
-- ORDER BY t.ticket_no, f.scheduled_departure

-- WITH duplicate AS (
-- SELECT t.passenger_name,
--     count(*) OVER (PARTITION BY t.passenger_name) name_count
-- FROM tickets t
-- )
-- SELECT DISTINCT d.*, round(d.name_count::numeric / count(*) OVER (), 5) name_ratio
-- FROM duplicate d
-- WHERE name_count > 1
-- ORDER BY d.name_count DESC
-- LIMIT 20

-- WITH duplicate AS (
-- SELECT substring(t.passenger_name, '^\w+') passenger_name,
--     count(*) OVER (PARTITION BY substring(t.passenger_name, '^\w+')) name_count
-- FROM tickets t
-- )
-- SELECT DISTINCT d.*, round(d.name_count::numeric / count(*) OVER (), 5) name_ratio
-- FROM duplicate d
-- ORDER BY d.name_count DESC
-- LIMIT 20

-- WITH ticket_route AS (
-- SELECT t.ticket_no,
--     array_agg(f.departure_airport ORDER BY f.scheduled_departure)
--         || (array_agg(f.arrival_airport ORDER BY f.scheduled_departure DESC))[1] route
-- FROM tickets t
--     JOIN ticket_flights tf ON tf.ticket_no = t.ticket_no
--     JOIN flights f ON f.flight_id = tf.flight_id
-- WHERE t.ticket_no IN ('0005432000987','0005432383484')
-- GROUP BY t.ticket_no
-- )
-- SELECT tr.*, route[1] origin,
--     route[cardinality(route) / 2 + 1] middle,
--     route[cardinality(route)] destination,
--     route[1] = route[cardinality(route)] round_trip
-- FROM ticket_route tr

-- WITH ticket_route AS (
-- SELECT t.ticket_no,
--     array_agg(f.departure_airport ORDER BY f.scheduled_departure) direct_route,
--     array_agg(f.arrival_airport ORDER BY f.scheduled_departure DESC) return_route
-- FROM tickets t
--     JOIN ticket_flights tf ON tf.ticket_no = t.ticket_no
--     JOIN flights f ON f.flight_id = tf.flight_id
-- WHERE t.ticket_no IN ('0005432000987','0005432383484')
-- GROUP BY t.ticket_no
-- )
-- SELECT tr.*, tr.direct_route = tr.return_route same_return
-- FROM ticket_route tr

WITH flight_schedule AS (
SELECT f.departure_airport departure, f.arrival_airport arrival,
    array_agg(extract(dow FROM f.scheduled_departure)
        ORDER BY f.scheduled_departure) schedule
FROM flights f
GROUP BY f.departure_airport, f.arrival_airport, extract(week FROM f.scheduled_departure)
)
SELECT DISTINCT
    d.departure dir_departure, d.arrival dir_arrival, d.schedule dir_schedule,
    r.departure ret_departure, r.arrival ret_arrival, r.schedule ret_schedule
FROM flight_schedule d, flight_schedule r
WHERE d.departure = r.arrival AND d.arrival = r.departure
    AND NOT (d.schedule && r.schedule)
ORDER BY d.departure
