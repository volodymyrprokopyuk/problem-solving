BEGIN;

SET search_path TO account;

\echo ** Service launch

\set legal_entity_id 'c9a68b87-664c-4170-a906-60d889f4247f'

INSERT INTO legal_entity (legal_entity_id, legal_entity_name)
VALUES (:'legal_entity_id', 'PagoFX UK');

TABLE legal_entity;

-- -- KYC rules

-- INSERT INTO kyc_rule (legal_entity_id, payment_type, payment_amount, currency_pair,
--     country_pair, payment_period, payment_value, payment_volume, risk, kyc_component,
--     rule_validity, description)
-- VALUES
--     (:'legal_entity_id', '^domesticx$', '[0, 1000]', '^GBPEUR$', '^UKES$',
--     '3 months', '[0, 1000]', '[0, 10]', '[0, 70]', '{basic, id}', '(,)',
--     'Domestic payments require id check'),
--     (:'legal_entity_id', '^domestic$', '[0, 1000]', '^GBPEUR$', '^UKES$',
--     '3 months', '[0, 1000]', '[0, 10]', '[30, 80]', '{basic, id, address}', '(,2020-01-01)',
--     '+ address check');

-- Customer sign up

\set account_id '5a334ae3-8319-41b5-a455-aa8d63e4b702'

INSERT INTO account (account_id, legal_entity_id, account_type, account_status)
VALUES (:'account_id', :'legal_entity_id', 'individual', 'active');

TABLE account;

\echo ** Before payment instruction

\set payment_type 'domestic'
\set payment_amount 200.0
\set currency_pair 'GBPEUR'
\set country_pair 'UKES'
\set payment_period '3 months'
\set payment_value 0.0
\set payment_volume 0
\set expiration '1 year'

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

INSERT INTO basic_info (account_id, first_name, last_name, birth_date, email, phone)
VALUES (:'account_id', 'Ana', 'Del Río', '1991-03-12', 'ana.perez@gmail.com', '654987321');
INSERT INTO basic_info (account_id, first_name, last_name, birth_date, email, phone,
    creation_ts)
VALUES (:'account_id', 'Ana María', 'Del Río', '1991-03-12', 'ana.perez@gmail.com', '654987321',
    clock_timestamp());

TABLE basic_info;

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

INSERT INTO risk_info (account_id, risk_score)
VALUES (:'account_id', 50);

TABLE risk_info;

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

\echo ** Before domestic payment

INSERT INTO id_info (account_id, id_type, id_number, valid_until)
VALUES (:'account_id', 'id_card', 'AB-321654', '2022-02-25');

TABLE id_info;

INSERT INTO risk_info (account_id, risk_score, creation_ts)
VALUES (:'account_id', 40, clock_timestamp());

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

\echo ** Before wallet payment

INSERT INTO address_info (account_id, country, region, city, street)
VALUES (:'account_id', 'Spain', 'Madrid', 'Villalba', 'Avenida de los Olivos, 45');

TABLE address_info;

INSERT INTO risk_info (account_id, risk_score, creation_ts)
VALUES (:'account_id', 35, clock_timestamp());

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

\echo ** Before international payment

INSERT INTO selfie_info (account_id, selfie_uri)
VALUES (:'account_id', 'https://selfie.com/photo/cba987.jpg');

TABLE selfie_info;

INSERT INTO risk_info (account_id, risk_score, creation_ts)
VALUES (:'account_id', 25, clock_timestamp());

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

\echo ** Before > 1000 GBP payment

INSERT INTO extra_info (account_id, occupation, income, source_of_funds)
VALUES (:'account_id', 'director', '[7e4, 8e4)', NULL);

TABLE extra_info;

INSERT INTO risk_info (account_id, risk_score, creation_ts)
VALUES (:'account_id', 20, clock_timestamp());

SELECT * FROM get_kyc_status(
    :'account_id', :'payment_type', :payment_amount, :'currency_pair', :'country_pair',
    :'payment_period', :payment_value, :payment_volume, :'expiration');

-- \set account_id 'bae92617-4cfa-42fe-a593-5dfa374f905a'

\echo ** Account information

SELECT * FROM get_account_info(:'account_id');

\echo ** Account history

SELECT jsonb_pretty(a.*) account_history FROM get_account_history(:'account_id') a;

ROLLBACK;
