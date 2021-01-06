-- SELECT c.*
-- FROM course c;

-- SELECT s.*
-- FROM student s;

-- SELECT DISTINCT s.start_year
-- FROM student s;

-- SELECT *
-- FROM student, course;

-- SELECT *
-- FROM student s
--     JOIN exam e ON e.student_id = s.student_id
--     JOIN course c ON c.course_id = e.course_id

-- SELECT *
-- FROM student s
--     LEFT JOIN exam e ON e.student_id = s.student_id
--         AND e.course_id = 'CS301';

-- SELECT *
-- FROM student s
--     JOIN exam e ON e.student_id = s.student_id
-- WHERE s.start_year > 2014;

-- SELECT e.*
-- FROM exam e
-- ORDER BY e.score, e.student_id, e.course_id DESC;

SELECT e.course_id, count(e.*) exam_count,
    count(DISTINCT e.student_id) student_count,
    round(avg(e.score), 3) score_average
FROM exam e
GROUP BY e.course_id
UNION ALL
SELECT 'all' course_id, count(e.*) exam_count,
    count(DISTINCT e.student_id) student_count,
    round(avg(e.score), 3) score_average
FROM exam e

-- WITH good_student AS (
--     SELECT e.student_id, count(e.student_id) exam_count, e.score
--     FROM exam e
--     WHERE e.score = 5
--     GROUP BY e.score, e.student_id
--     HAVING count(e.*) > 1
-- )
-- SELECT s.student_id, s.first_name, gs.exam_count, gs.score
-- FROM good_student gs
--     JOIN student s ON s.student_id = gs.student_id;
