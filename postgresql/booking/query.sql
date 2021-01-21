-- SELECT f.flight_no, bp.seat_no, t.passenger_name, b.book_date
-- FROM flights f
--     JOIN boarding_passes bp ON bp.flight_id = f.flight_id
--     JOIN tickets t ON t.ticket_no = bp.ticket_no
--     JOIN bookings b ON b.book_ref = t.book_ref
-- WHERE f.departure_airport = 'SVO' AND f.arrival_airport = 'OVB'
--     AND f.actual_departure::date = bookings.now()::date - interval '2 days'
--     AND bp.seat_no = '1A'

SELECT f.flight_no, f.aircraft_code, bp.seat_no, count(*) OVER ()
FROM flights f
    JOIN seats s ON s.aircraft_code = f.aircraft_code
    LEFT JOIN boarding_passes bp
        ON bp.flight_id = f.flight_id AND bp.seat_no = s.seat_no
WHERE f.actual_departure::date = bookings.now()::date - interval '1 day'
    AND f.flight_no = 'PG0404' AND bp.seat_no IS NULL
LIMIT 1
