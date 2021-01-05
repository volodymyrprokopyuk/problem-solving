INSERT INTO course (course_id, title, hours) VALUES
    ('CS301', 'Databases', 30),
    ('CS305', 'Networks', 60);

INSERT INTO student (student_id, first_name, start_year) VALUES
    (1451, 'Anna', 2014),
    (1432, 'Victor', 2014),
    (1556, 'Nina', 2015);

INSERT INTO exam (student_id, course_id, score) VALUES
    (1451, 'CS301', 5),
    (1556, 'CS301', 5),
    (1451, 'CS305', 5),
    (1432, 'CS305', 4);
