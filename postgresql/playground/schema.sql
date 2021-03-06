-- BEGIN;

-- CREATE TABLE course (
--     course_id text NOT NULL,
--     title text NOT NULL,
--     hours integer NOT NULL,
--     CONSTRAINT pk_course PRIMARY KEY (course_id)
-- );

-- CREATE TABLE sgroup (
--     sgroup_id text NOT NULL,
--     student_id integer,
--     CONSTRAINT pk_sgroup PRIMARY KEY (sgroup_id)
-- );

-- CREATE TABLE student (
--     student_id integer NOT NULL,
--     sgroup_id text NOT NULL,
--     first_name text NOT NULL,
--     start_year integer NOT NULL,
--     CONSTRAINT pk_student PRIMARY KEY (student_id),
--     CONSTRAINT fk_sgroup_of_student FOREIGN KEY (sgroup_id)
--         REFERENCES sgroup(sgroup_id)
-- );

-- ALTER TABLE sgroup
-- ADD CONSTRAINT fk_monitor_of_sgroup FOREIGN KEY (student_id)
--     REFERENCES student(student_id);

-- CREATE TABLE exam (
--     student_id integer NOT NULL,
--     course_id text NOT NULL,
--     score integer NOT NULL,
--     CONSTRAINT fk_exam_of_student FOREIGN KEY (student_id)
--         REFERENCES student(student_id),
--     CONSTRAINT fk_exam_of_course FOREIGN KEY (course_id)
--         REFERENCES course(course_id)
-- );

-- CREATE OR REPLACE FUNCTION duplicate (x integer) RETURNS integer LANGUAGE sql AS $$
--     SELECT x * 2;
-- $$;

-- COMMIT;

-- BEGIN;

-- CREATE EXTENSION hstore;

-- SELECT h.e entry, h.e -> 'a' "key", h.e -> '{a, c}'::text[] keys
-- FROM (VALUES ('a => 1, b => 2'::hstore), ('c => 3, d => 4')) h(e);

-- ROLLBACK;

BEGIN;

CREATE EXTENSION pg_trgm;

ROLLBACK;
