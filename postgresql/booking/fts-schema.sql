CREATE TABLE lecture_note (
    course text NOT NULL,
    lecture_number text NOT NULL,
    lecture_title text NOT NULL,
    lecture_content text NOT NULL,
    ts_lecture_content tsvector,
    ts_title_content tsvector,
    CONSTRAINT pk_lecture_note PRIMARY KEY (course, lecture_number));

INSERT INTO lecture_note (course, lecture_number, lecture_title, lecture_content)
VALUES ('CS301', 'I', 'Базы данных', 'С этой главы начинается наше знакомство '
        || 'с увлекательным миром баз данных'),
    ('CS301', 'II', 'Первые шаги', 'Продолжаем знакомство с миром баз данных. '
        || 'Создадим нашу первую текстовую базу данных'),
    ('CS305', 'I', 'Локальные сети', 'Здесь начнется наше полное приключений '
        || 'путешествие в интригующий мир сетей');

UPDATE lecture_note
SET ts_lecture_content = to_tsvector('russian', lecture_content),
    ts_title_content = setweight(to_tsvector('russian', lecture_title), 'B')
        || ' ' || setweight(to_tsvector('russian', lecture_content), 'D');
