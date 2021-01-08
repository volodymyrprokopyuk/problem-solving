INSERT INTO course (course_id, title, hours) VALUES
    ('CS301', 'Databases', 30),
    ('CS305', 'Networks', 60);

INSERT INTO sgroup (sgroup_id, student_id) VALUES
    ('A-101', NULL);

INSERT INTO student (student_id, sgroup_id, first_name, start_year) VALUES
    (1451, 'A-101', 'Anna', 2014),
    (1432, 'A-101', 'Victor', 2014),
    (1556, 'A-101', 'Nina', 2015);

UPDATE sgroup SET student_id = 1451 WHERE sgroup_id = 'A-101';

INSERT INTO exam (student_id, course_id, score) VALUES
    (1451, 'CS301', 5),
    (1556, 'CS301', 5),
    (1451, 'CS305', 5),
    (1432, 'CS305', 4);
