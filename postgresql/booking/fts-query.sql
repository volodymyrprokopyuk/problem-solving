-- SELECT course, lecture_title, lecture_content
-- FROM lecture_note
-- WHERE lecture_content ~~ '%базы данных%'

-- SELECT course, lecture_title, lecture_content
-- FROM lecture_note
-- WHERE ts_lecture_content @@ to_tsquery('russian', 'базы & данные')

-- SELECT course, lecture_title, lecture_content,
--     ts_rank_cd('{0.1, 0.0, 1.0, 0.0}', ts_title_content, ts_query)
-- FROM lecture_note, to_tsquery('russian', 'базы & данные') ts_query
-- WHERE ts_title_content @@ ts_query
-- ORDER BY ts_rank_cd DESC
