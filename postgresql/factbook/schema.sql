BEGIN;

CREATE TABLE factbook (
    year integer,
    date date,
    shares text,
    trades text,
    dollars text);

\copy factbook FROM 'factbook/factbook.csv' WITH DELIMITER ' ' NULL ''

ALTER TABLE factbook
    ALTER shares TYPE bigint USING replace(shares, ',', '')::bigint,
    ALTER trades TYPE bigint USING replace(trades, ',', '')::bigint,
    ALTER dollars TYPE bigint USING substring(replace(dollars, ',', '') FROM 2)::bigint;

COMMIT;
