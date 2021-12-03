# ODS inquiry API security POC

## Introduction

The present ODS inquiry API security POC implementes OAuth2 opaque access token
and JSON web tokne (JWT) flows for the OAuth2 client credentials authorization
grant.

## Architecture overview

- [Security architecture](https://jira.iconsolutions.com/confluence/display/IPFV2/ODS+-+Inquiry+and+search+API#heading-ODSAPIsecurityarchitecture)
- [Access flows sequence diagrams](https://jira.iconsolutions.com/confluence/display/IPFV2/ODS+-+Inquiry+and+search+API#heading-ODSAPIsecurityPOC)

## Use case

The present ODS inquiry API security POC implementes the use case where a
confidential client (a web application with a backend that can securely store
secrets like client credentials or a web service) accesses a protected resource
(a data service DS) on its own behalf by presenting an access token (both opaque
token or JWT). The access to the DS is mediated via the identity and access
proxy (IAP). The authorization is managed by the token management (TM) OAuth2
authorization server via issuance and introspection of access tokens.

## Main components

- **Client** is a web application or a web service (e. g. IPF ODS UI, bank's
  system that accesses ODS data)
- **Identity and access proxy** (IAP) is the single entry point to entry to
  access the DS (ORY Oathkeepr)
- **Token management** (TM) OAuth2 authorizaiton server issues and introspects
  access tokens (ORY Hydra)
- **Data service** (DS) is the ODS inquiry API that releases data only to
  authorized clients that present valid acces tokens

## Usage

- Initialize Docker infrastructure and generate TLS/JWKS key pairs
    ```zsh
    ./bin/ods.sh -i
    ```
- Start TM
    ```zsh
    ./bin/ods.sh -h
    ```
- Install TM certificates (new terminal)
    ```zsh
    ./bin/ods.sh -hc
    ```
- Start IAP
    ```zsh
    ./bin/ods.sh -o
    ```
- Install IAP certificates (new terminal)
    ```zsh
    ./bin/ods.sh -oc
    ```
- Install TM keys
    ```zsh
    ./bin/ods.sh -hk
    ```
- Import TM clients
    ```zsh
    ./bin/ods.sh -c
    ```
- Start DS
    ```zsh
    ./bin/ods.sh -d
    ```
- Install DS certificates (new terminal)
    ```zsh
    ./bin/ods.sh -dc
    ```
- Perform OAuth2 opaque access token flow
    ```zsh
    ./bin/ods.sh -t
    ```
- Perform OAuth2 JWT flow
    ```zsh
    ./bin/ods.sh -j
    ```

## Dependencies

- Docker LXC management
- OpenSSL TLS toolkit
- rhonabwy JOSE toolkit
- curl HTTP client
- jq JSON manipulation
