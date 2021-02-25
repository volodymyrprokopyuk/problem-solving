BEGIN;

CREATE SCHEMA account;

SET search_path TO account;

-- Data model

CREATE TABLE legal_entity (
    legal_entity_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    legal_entity_name text NOT NULL CHECK (legal_entity_name ~ '^[ \w]{3,}$'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TYPE account_type_t AS ENUM ('individual', 'sole_trader', 'sme');
CREATE TYPE account_status_t AS ENUM ('active', 'blocked', 'closed');

CREATE TABLE account (
    account_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    legal_entity_id uuid NOT NULL REFERENCES legal_entity (legal_entity_id),
    account_type account_type_t NOT NULL,
    account_status account_status_t NOT NULL,
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TABLE basic_info (
    basic_info_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES account (account_id),
    first_name text NOT NULL CHECK (first_name ~ '^[ \w]{3,}$'),
    last_name text NOT NULL CHECK (last_name ~ '^[ \w]{3,}$'),
    birth_date date NOT NULL CHECK (age(birth_date) < '80 years'),
    email text NOT NULL CHECK (email ~ '^\S{3,}@\S{3,}$'),
    phone text NOT NULL CHECK (phone ~ '^\d{9,}$'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TYPE risk_level_t AS ENUM ('low', 'medium', 'high');

CREATE TABLE risk_info (
    risk_info_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES account (account_id),
    risk_score integer NOT NULL CHECK (0 <= risk_score AND risk_score <= 100),
    risk_level risk_level_t NOT NULL GENERATED ALWAYS AS (CASE
        WHEN risk_score <@ '[0, 30)'::int4range THEN 'low'::risk_level_t
        WHEN risk_score <@ '[30, 70)'::int4range THEN 'medium'::risk_level_t
        ELSE 'high'::risk_level_t END) STORED,
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TYPE id_type_t AS ENUM ('passport', 'id_card', 'driving_license');

CREATE TABLE id_info (
    id_info_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES account (account_id),
    id_type id_type_t NOT NULL,
    id_number text NOT NULL CHECK (id_number ~ '[- \w]{5,}'),
    valid_until date NOT NULL CHECK (age(valid_until, current_date) < '15 years'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TABLE address_info (
    address_info_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES account (account_id),
    country text NOT NULL CHECK (country ~ '^[- \w]{3,}$'),
    region text NOT NULL CHECK (region ~ '^[- \w]{3,}$'),
    city text NOT NULL CHECK (city ~ '^[- \w]{3,}$'),
    street text NOT NULL CHECK (street ~ '^[-, \w]{3,}$'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TABLE selfie_info (
    selfie_info_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES account (account_id),
    selfie_uri text NOT NULL CHECK (selfie_uri ~ '\w{3,}'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

CREATE TABLE extra_info (
    extra_info_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES account (account_id),
    occupation text CHECK (occupation ~ '^[ \w]{3,}$'),
    income numrange,
    source_of_funds text CHECK (source_of_funds ~ '^[ \w]{3,}$'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp,
    CHECK (coalesce(occupation, income::text, source_of_funds) IS NOT NULL));

CREATE TYPE kyc_component_t AS ENUM ('basic', 'id', 'address', 'selfie', 'extra', 'risk');

CREATE TABLE kyc_rule (
    kyc_rule_id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    legal_entity_id uuid NOT NULL REFERENCES legal_entity (legal_entity_id),
    payment_type text NOT NULL -- DEFAULT '^\w+$'
        CHECK (regexp_match('', payment_type) IS NULL),
    payment_amount numrange NOT NULL -- DEFAULT '[0, 1e10)'
        CHECK (lower(payment_amount) >= 0 AND upper(payment_amount) > 0),
    currency_pair text NOT NULL -- DEFAULT '^\w{6}$'
        CHECK (regexp_match('', currency_pair) IS NULL),
    country_pair text NOT NULL -- DEFAULT '^\w{4}$'
        CHECK (regexp_match('', country_pair) IS NULL),
    payment_period interval NOT NULL, -- DEFAULT '1 century',
    payment_value numrange NOT NULL -- DEFAUlT '[0, 1e10)'
        CHECK (lower(payment_value) >= 0 AND upper(payment_value) > 0),
    payment_volume int4range NOT NULL -- DEFAULT '[0, 1000)'
        CHECK (lower(payment_volume) >= 0 AND upper(payment_volume) > 0),
    risk int4range NOT NULL -- DEFAULT '[0, 100]'
        CHECK (lower(risk) >= 0 AND upper(risk) <= 101),
    kyc_component kyc_component_t[] NOT NULL --DEFAULT '{basic, id, address, selfie, extra}'
        CHECK (array_length(kyc_component, 1) > 0),
    rule_validity tstzrange NOT NULL DEFAULT '(,)',
    description text NOT NULL -- DEFAULT 'By default full KYC is required and any risk is acceptable'
        CHECK (description ~ '^.{5,}$'),
    creation_ts timestamptz NOT NULL DEFAULT current_timestamp);

-- Data access interface

CREATE FUNCTION range_intersect (a anyrange, b anyrange)
RETURNS anyrange LANGUAGE SQL IMMUTABLE AS $$ SELECT a * b; $$;

CREATE AGGREGATE range_intersect_agg (anyrange) (
    SFUNC = range_intersect, STYPE = anyrange, INITCOND = '(,)');

CREATE TYPE kyc_status_t AS
ENUM ('pending', 'up_to_date', 'outdated', 'acceptable', 'unacceptable');

CREATE TYPE payment_type AS ENUM ('domestic', 'wallet', 'international');

CREATE FUNCTION get_kyc_status (
    a_account_id uuid,
    a_payment_type payment_type,
    a_payment_amount numeric,
    a_currency_pair text,
    a_country_pair text,
    a_payment_period interval,
    a_payment_value numeric,
    a_payment_volume integer,
    a_expiration interval DEFAULT '1 year')
RETURNS TABLE (
    kyc_component kyc_component_t,
    kyc_status kyc_status_t,
    kyc_reason text)
LANGUAGE SQL AS $$
WITH matched_rule AS (
    SELECT r.kyc_component, r.risk, r.description kyc_reason
    FROM kyc_rule r
    WHERE r.legal_entity_id = (
        SELECT a.legal_entity_id FROM account a WHERE a.account_id = a_account_id)
        AND a_payment_type::text ~* r.payment_type
        AND a_payment_amount <@ r.payment_amount
        AND a_currency_pair ~* r.currency_pair
        AND a_country_pair ~* r.country_pair
        AND a_payment_period <= r.payment_period
        AND a_payment_value <@ r.payment_value
        AND a_payment_volume <@ r.payment_volume
        AND current_timestamp <@ r.rule_validity),
default_rule AS (
    SELECT r.kyc_component, r.risk, r.kyc_reason
    FROM matched_rule r
    UNION ALL
    SELECT '{basic, id, address, selfie, extra}' kyc_component, '[0, 100]' risk,
        'By default full KYC is required and any risk is acceptable' kyc_reason
    FROM (VALUES (1)) t
    WHERE NOT EXISTS (SELECT 1 FROM matched_rule)),
distinct_rule AS (
    SELECT array_agg(DISTINCT c) kyc_component,
        range_intersect_agg(r.risk) risk,
        string_agg(DISTINCT r.kyc_reason, '. ') kyc_reason
    FROM default_rule r, unnest(r.kyc_component) c),
final_rule AS (
    SELECT c kyc_component, r.risk, r.kyc_reason
    FROM distinct_rule r, unnest(r.kyc_component) c)
-- basic_info
SELECT r.kyc_component, CASE
    WHEN b.account_id IS NULL THEN 'pending'::kyc_status_t
    ELSE 'up_to_date'::kyc_status_t END kyc_status, r.kyc_reason
FROM final_rule r LEFT JOIN (
    SELECT b.* FROM basic_info b WHERE b.account_id = a_account_id
    ORDER BY b.creation_ts DESC LIMIT 1) b ON TRUE
WHERE r.kyc_component = 'basic'
UNION ALL
-- id_info
SELECT r.kyc_component, CASE
    WHEN i.account_id IS NULL THEN 'pending'::kyc_status_t
    WHEN age(i.valid_until) > a_expiration THEN 'outdated'::kyc_status_t
    ELSE 'up_to_date'::kyc_status_t END kyc_status, r.kyc_reason
FROM final_rule r LEFT JOIN (
    SELECT i.* FROM id_info i WHERE i.account_id = a_account_id
    ORDER BY i.creation_ts DESC LIMIT 1) i ON TRUE
WHERE r.kyc_component = 'id'
UNION ALL
-- address_info
SELECT r.kyc_component, CASE
    WHEN a.account_id IS NULL THEN 'pending'::kyc_status_t
    WHEN age(a.creation_ts) > a_expiration THEN 'outdated'::kyc_status_t
    ELSE 'up_to_date'::kyc_status_t END kyc_status, r.kyc_reason
FROM final_rule r LEFT JOIN (
    SELECT a.* FROM address_info a WHERE a.account_id = a_account_id
    ORDER BY a.creation_ts DESC LIMIT 1) a ON TRUE
WHERE r.kyc_component = 'address'
UNION ALL
-- selfie_info
SELECT r.kyc_component, CASE
    WHEN s.account_id IS NULL THEN 'pending'::kyc_status_t
    WHEN age(s.creation_ts) > a_expiration THEN 'outdated'::kyc_status_t
    ELSE 'up_to_date'::kyc_status_t END kyc_status, r.kyc_reason
FROM final_rule r LEFT JOIN (
    SELECT s.* FROM selfie_info s WHERE s.account_id = a_account_id
    ORDER BY s.creation_ts DESC LIMIT 1) s ON TRUE
WHERE r.kyc_component = 'selfie'
UNION ALL
-- extra_info
SELECT r.kyc_component, CASE
    WHEN e.account_id IS NULL THEN 'pending'::kyc_status_t
    WHEN age(e.creation_ts) > a_expiration THEN 'outdated'::kyc_status_t
    ELSE 'up_to_date'::kyc_status_t END kyc_status, r.kyc_reason
FROM final_rule r LEFT JOIN (
    SELECT e.* FROM extra_info e WHERE e.account_id = a_account_id
    ORDER BY e.creation_ts DESC LIMIT 1) e ON TRUE
WHERE r.kyc_component = 'extra'
UNION ALL
-- risk
(SELECT 'risk' kyc_component, CASE
    WHEN a.account_id IS NULL THEN 'pending'::kyc_status_t
    WHEN a.risk_score <@ r.risk THEN 'acceptable'::kyc_status_t
    ELSE 'unacceptable'::kyc_status_t END kyc_stauts,
    'Risk intersaction from all applicable KYC rules' kyc_reason
FROM final_rule r LEFT JOIN (
    SELECT r.* FROM risk_info r WHERE r.account_id = a_account_id
    ORDER BY r.creation_ts DESC LIMIT 1) a ON TRUE
LIMIT 1);
$$;

CREATE FUNCTION get_account_info (a_account_id uuid)
RETURNS TABLE (
    account_id uuid,
    account_type account_type_t,
    account_status account_status_t,
    account_ts timestamptz,
    first_name text,
    last_name text,
    birth_date date,
    email text,
    phone text,
    basic_ts timestamptz,
    id_type id_type_t,
    id_number text,
    valid_until date,
    id_ts timestamptz,
    country text,
    region text,
    city text,
    street text,
    address_ts timestamptz,
    selfie_uri text,
    selfie_ts timestamptz,
    occupation text,
    income numrange,
    source_of_funds text,
    extra_ts timestamptz,
    risk_score numeric,
    risk_level risk_level_t,
    risk_ts timestamptz)
LANGUAGE SQL AS $$
SELECT a.account_id, a.account_type, a.account_status, a.creation_ts account_ts,
    b.first_name, b.last_name, b.birth_date, b.email, b.phone, b.creation_ts basic_ts,
    i.id_type, i.id_number, i.valid_until, i.creation_ts id_ts,
    d.country, d.region, d.city, d.street, d.creation_ts address_ts,
    s.selfie_uri, s.creation_ts selfie_ts,
    e.occupation, e.income, e.source_of_funds, e.creation_ts extra_ts,
    r.risk_score, r.risk_level, r.creation_ts risk_ts
FROM account a
    LEFT JOIN (
    SELECT b.* FROM basic_info b WHERE b.account_id = a_account_id
    ORDER BY b.creation_ts DESC LIMIT 1) b ON TRUE
    LEFT JOIN (
    SELECT i.* FROM id_info i WHERE i.account_id = a_account_id
    ORDER BY i.creation_ts DESC LIMIT 1) i ON TRUE
    LEFT JOIN (
    SELECT d.* FROM address_info d WHERE d.account_id = a_account_id
    ORDER BY d.creation_ts DESC LIMIT 1) d ON TRUE
    LEFT JOIN (
    SELECT s.* FROM selfie_info s WHERE s.account_id = a_account_id
    ORDER BY s.creation_ts DESC LIMIT 1) s ON TRUE
    LEFT JOIN (
    SELECT e.* FROM extra_info e WHERE e.account_id = a_account_id
    ORDER BY e.creation_ts DESC LIMIT 1) e ON TRUE
    LEFT JOIN (
    SELECT r.* FROM risk_info r WHERE r.account_id = a_account_id
    ORDER BY r.creation_ts DESC LIMIT 1) r ON TRUE
WHERE a.account_id = a_account_id;
$$;

CREATE FUNCTION get_account_history (
    a_account_id uuid,
    a_history_limit integer DEFAULT 10)
RETURNS jsonb
LANGUAGE SQL AS $$
WITH basic_history AS (
    SELECT jsonb_build_object(
        'first_name', b.first_name, 'last_name', b.last_name,
        'birth_date', b.birth_date, 'email', b.email, 'phone', b.phone,
        'basic_ts', b.creation_ts) basic_entry
    FROM basic_info b WHERE b.account_id = a_account_id
    ORDER BY b.creation_ts DESC LIMIT a_history_limit),
basic_history_agg AS (
    SELECT jsonb_agg(b.basic_entry) basic_entry
    FROM basic_history b),
id_history AS (
    SELECT jsonb_build_object(
        'id_type', i.id_type, 'id_number', i.id_number,
        'valid_until', i.valid_until, 'id_ts', i.creation_ts) id_entry
    FROM id_info i WHERE i.account_id = a_account_id
    ORDER BY i.creation_ts DESC LIMIT a_history_limit),
id_history_agg AS (
    SELECT jsonb_agg(i.id_entry) id_entry
    FROM id_history i),
address_history AS (
    SELECT jsonb_build_object(
        'country', a.country, 'region', a.region, 'city', a.city, 'street', a.street,
        'address_ts', a.creation_ts) address_entry
    FROM address_info a WHERE a.account_id = a_account_id
    ORDER BY a.creation_ts DESC LIMIT a_history_limit),
address_history_agg AS (
    SELECT jsonb_agg(a.address_entry) address_entry
    FROM address_history a),
selfie_history AS (
    SELECT jsonb_build_object(
        'selfie_uri', s.selfie_uri, 'selfie_ts', s.creation_ts) selfie_entry
    FROM selfie_info s WHERE s.account_id = a_account_id
    ORDER BY s.creation_ts DESC LIMIT a_history_limit),
selfie_history_agg AS (
    SELECT jsonb_agg(s.selfie_entry) selfie_entry
    FROM selfie_history s),
extra_history AS (
    SELECT jsonb_build_object(
        'occupation', e.occupation, 'income', e.income,
        'source_of_funds', e.source_of_funds, 'extra_ts', e.creation_ts) extra_entry
    FROM extra_info e WHERE e.account_id = a_account_id
    ORDER BY e.creation_ts DESC LIMIT a_history_limit),
extra_history_agg AS (
    SELECT jsonb_agg(s.extra_entry) extra_entry
    FROM extra_history s),
risk_history AS (
    SELECT jsonb_build_object(
        'risk_score', r.risk_score, 'risk_level', r.risk_level,
        'risk_ts', r.creation_ts) risk_entry
    FROM risk_info r WHERE r.account_id = a_account_id
    ORDER BY r.creation_ts DESC LIMIT a_history_limit),
risk_history_agg AS (
    SELECT jsonb_agg(s.risk_entry) risk_entry
    FROM risk_history s)
SELECT jsonb_build_object(
    'account_id', a.account_id, 'account_type', a.account_type,
    'account_status', a.account_status, 'account_ts', a.creation_ts,
    'basic_history', b.basic_entry, 'id_history', i.id_entry,
    'address_history', d.address_entry, 'selfie_history', s.selfie_entry,
    'extra_history', e.extra_entry, 'risk_history', r.risk_entry) account_history
FROM account a, basic_history_agg b, id_history_agg i, address_history_agg d,
    selfie_history_agg s, extra_history_agg e, risk_history_agg r
WHERE a.account_id = a_account_id;
$$;

-- Service launch

\set legal_entity_id 'c9a68b87-664c-4170-a906-60d889f4247f'

INSERT INTO legal_entity (legal_entity_id, legal_entity_name)
VALUES (:'legal_entity_id', 'PagoFX UK');

-- KYC rules

INSERT INTO kyc_rule (legal_entity_id, payment_type, payment_amount, currency_pair,
    country_pair, payment_period, payment_value, payment_volume, risk, kyc_component,
    rule_validity, description)
VALUES
    (:'legal_entity_id', '^domesticx$', '[0, 1000]', '^GBPEUR$', '^UKES$',
    '3 months', '[0, 1000]', '[0, 10]', '[0, 70]', '{basic, id}', '(,)',
    'Domestic payments require id check'),
    (:'legal_entity_id', '^domestic$', '[0, 1000]', '^GBPEUR$', '^UKES$',
    '3 months', '[0, 1000]', '[0, 10]', '[30, 80]', '{basic, id, address}', '(,2020-01-01)',
    '+ address check');

-- Customer sign up

\set account_id '5a334ae3-8319-41b5-a455-aa8d63e4b702'

INSERT INTO account (account_id, legal_entity_id, account_type, account_status)
VALUES (:'account_id', :'legal_entity_id', 'individual', 'active');

INSERT INTO basic_info (account_id, first_name, last_name, birth_date, email, phone)
VALUES (:'account_id', 'Ana', 'Perez', '1991-03-12', 'ana.perez@gmail.com', '654987321');
INSERT INTO basic_info (account_id, first_name, last_name, birth_date, email, phone, creation_ts)
VALUES (:'account_id', 'Ana María', 'Perez', '1991-03-12', 'ana.perez@gmail.com', '654987321',
    clock_timestamp());

INSERT INTO risk_info (account_id, risk_score)
VALUES (:'account_id', 50);
INSERT INTO risk_info (account_id, risk_score, creation_ts)
VALUES (:'account_id', 80, clock_timestamp());

-- Before domestic payment

INSERT INTO id_info (account_id, id_type, id_number, valid_until)
VALUES (:'account_id', 'id_card', 'AB-321654', '2020-02-23');

-- Before wallet payment

INSERT INTO address_info (account_id, country, region, city, street)
VALUES (:'account_id', 'Spain', 'Madrid', 'Villalba', 'Avenida de los Olivos, 45');

-- Before international payment

INSERT INTO selfie_info (account_id, selfie_uri)
VALUES (:'account_id', 'https://selfie.com/photo/cba987.jpg');

-- Before > 1000 GBP payment

INSERT INTO extra_info (account_id, occupation, income, source_of_funds)
VALUES (:'account_id', 'director', '[50, 60]', NULL);

-- Before payment instruction

\set payment_type 'domestic'
\set payment_amount 200.0
\set currency_pair 'GBPEUR'
\set country_pair 'UKES'
\set payment_period '3 months'
\set payment_value 0.0
\set payment_volume 0
\set expiration '1 year'

-- \set account_id 'bae92617-4cfa-42fe-a593-5dfa374f905a'

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

SELECT * FROM get_account_info(:'account_id');

-- SELECT jsonb_pretty(a.*) account_history FROM get_account_history(:'account_id') a;

ROLLBACK;
