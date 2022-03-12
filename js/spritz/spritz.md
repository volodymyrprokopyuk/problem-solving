<!--
pandoc -f markdown -t html5 -s --self-contained --toc < spritz.md > spritz.html
-->
---
title: Spritz Engineering — Applicant Screening Questions
author: Volodymyr Prokopyuk (Vlad)
date: 2022-03-12

---

## Question 1

Given data in the following shape:

```js
const officers = [
  {age: 20, name: "Captain Piett"},
  {age: 23, name: "General Veers"},
  {age: 57, name: "General Ozzel"},
  {age: 88, name: "Commander Jerjerrod"}
];
```

Write statements in ES6 functional style (`.filter`, `.map`, `.reduce`, etc.)
which:

- Returns the sum of all ages of all officers
- Returns the name of the oldest officer
- Returns the average age of all generals

### Response 1

```js
/*
 * Assumptions
 * - The `officers` array is not empty
 * - Every object in the array has the `age` and `name` fields
 * - The `age` and `name` fields have valid values
 * - The `name` of a general starts with "General ..."
 */

const officers = [
  {age: 20, name: "Captain Piett"},
  {age: 23, name: "General Veers"},
  {age: 57, name: "General Ozzel"},
  {age: 88, name: "Commander Jerjerrod"}
];

// Returns the sum of all ages of all officers
const ageSum = officers.reduce((ageSum, {age: age}) => ageSum + age, 0)
console.log("Sum of all ages of all officers: %d", ageSum)
// Result: Sum of all ages of all officers: 188

// Returns the name of the oldest officer
const oldest = officers.sort(({age: aAge}, {age: bAge}) => bAge - aAge)[0]
console.log("The older officer: %s", oldest.name)
// Result: The older officer: Commander Jerjerrod

// Returns the average age of all generals
const generals = officers.filter(({name: name}) => name.startsWith("General"))
const ageAvg = generals.reduce((ageSum, {age: age}) => ageSum + age, 0) /
      generals.length
console.log("The average age of all generals: %d", ageAvg)
// Result: The average age of all generals: 40
```

## Question 2

What is the most interesting or challenging software engineering problem you’ve
worked on? Why?

### Response 2

#### Instant payment engine design

I participated in the design of an instant payment engine for a global tier 1
bank integrating different payment schemes with core banking systems and adding
complex business logic on top of it. I learned a lot about payment schemes and
messaging standards, core banking workflows, instant payment engine internals
(e. g. payment tracking, resilience mechanisms)

#### Security architecture of payment engine integration with core banking systems

I designed the security architecture of payment engine integration with core
banking systems. I learned a lot about security design principles, standard
protocols (e. g. OAuth2, OpenID Connect, SAML2) and best practices, as well as
common system integration protocols (e. g. REST, Kafka) and how to protect them
effectively

## Question 3

We have an authorization service which integrates with AWS Cognito. Cognito
maintains a record for every user, along with some basic details about them
(email, phone). We also have a users service backed by MongoDB as a primary
datastore. Most operations in the auth microservice involve calling the users
microservice synchronously to update data, and occasionally data will need to be
synced in the other direction as well. There is basically a 1:1 relationship
between the entities in both datastores.

Does it make sense to implement these in one service which keeps both data
stores reconciled, or two separate services with message passing between them?

### Response 3

The below two options may be valid depending on the wider context and specific
requirements. Below follows my reasoning for each option along with some
assumptions

#### Option 1. Merge the auth and user services with a reconciled datastore

Assumptions

- Most operations in the auth service imply a sync call to the user service
- The auth and user services are tightly coupled and always collaborate to
  provide the auth function

Given the above assumptions it makes sense to merge both services in order to
get a single source of truth for the augmented user records and reduce the
response time of the combined service which, in turn, supports the principle of
psychological acceptability of security controls by users allowing them to
naturally and correctly apply protection on resources. The extensibility and
flexibility of the Cognito identity store may become a problem if the required
user information cannot be accurately stored and efficiently retrieved from
Cognito

#### Option 2. Keep the auth and user services separated with message passing integration

Assumptions

- Only a small subset of the auth service operations implies a sync call to the
  user service
- The auth and user services are loosely coupled and perform different functions
  (e. g. user authN vs user KYC)

Given the above assumptions it makes sense to keep the auth and user services
separated following the separation of concerns and modular design principles
where the auth service effectively provides the user authN function and the user
service provides the user KYC function. Each service can evolve independently,
the design follows the least privilege (via system segmentation) and the least
common mechanism (separate access to the authN and KYC functions) security
principles. The design of the trust boundary protection (message passing
integration) between the auth and user services should take into account nature
of each operation and sensitivity of the involved data

## Question 4

Our platform will execute operations with customer funds that involve several
asynchronous steps, touching many services in a distributed environment
(liquidation of positions in DeFi protocols, currency conversion, sending
payment instructions to an ACH payment processor).

What mechanism would you design to reliably carry out the process, handling
failures and rollback of the transaction if necessary? Does it make sense to use
an ACID-compliant database?

### Response 4

#### Reliability and resilience mechanisms in a distributed environment

#### Option 1. In-process resilience

Define (potentially in a DSL) a business flow for each operation fully covering
all decisions, loops all edge cases. Transform the business flow into code
ensuring correct handling of errors/exceptions, performing retries with
logarithmic backoff, using the bulkhead design pattern where appropriate,
implementing 2-phase transaction commit and rollback (quite complex and usually
discouraged). All the above in-process resilience mechanisms depend on the
running process and may fail if the process crashes

#### Option 2. State persistence in a database

Persisting processing state in a database could recover the processing state
after a process crash, but relies on an external data store and usually implies
mutable state (e. g. steps of transaction processing) that could be a source of
subtle errors in a distributed system

#### Option 3. Event sourcing with compensation actions

Removes the mutable state from the equation by providing an append-only data
store of transaction processing events with compensation actions (new set of
events on top of exciting events) when transaction reversal is needed. Usually
implies event streaming (e. g. Kafka) or message queues (e. g. RabbitMQ).
Complexity lies in managing huge amount of events and eventual consistency

#### Option 4. Actor model resilience

Build a hierarchy of actors (workers and supervisors) deployed to a cluster of
nodes and managed by an actor runtime (e. g. Akka, OTP). Worker actors have
mailboxes of tasks, perform simple, well defined actions contributing to
transaction processing and report the results to a supervisor. Supervisor actor
tracks the status of transaction processing, handles failures and recovery
actions. There are multiple hierarchical levels of supervision. The actor
runtime introduces considerable performance overhead, non-trivial complexity and
non-determinism in the execution model (e. g. tasks are executed somewhere in
the cluster at the time that the runtime decides)

#### Option 5. Reactor model with an external scheduler

Implement the defined business flow as a finite-state machine (FSM) triggered by
an external scheduler. Each transaction processing step is initiated by a
scheduler by invoking the appropriate FSM action with a corresponding message.
The FSM action retrieves inputs and state from a database or other services,
performs business logic, stores the outputs and state in a database or other
services and while doing each of the above registers new scheduling events for
itself with an external scheduler (e. g. check liquidation of position in DeFi
protocols in 2 sec, retry sending a payment instruction to the ACH in 5 min).
The benefits of a reactor + schedule design are: 1) simple stateless FSM reactor
focusing on business logic 2) simple, but reliable scheduler with scheduling
events acknowledge and retry 3) separation of business logic (in the reactor)
and control and recovery logic (in the scheduler)

#### Use of ACID-compliant database

In financial, technological environments ACID-compliant databases make sense due
to data integrity checks, transaction isolation, strict consistency,
correctness, durability and flexible querying. The misconception consists of
using a single ACID-compliant database in a big distributed system where
scalability problems arise. The idea is to use multiple ACID-compliant databases
(e. g. per microservice) with possible data sharding and potential CQRS to
correctly manage transactional data without exceeding database limits.

## Question 5

What tools, practices, or frameworks do you find indispensable for development,
both frontend and backend?

### Response 5

Rather than naming concrete tools and frameworks which usually change with time
specifically in JavaScript / TypeScript ecosystem, I’d highlight a limited set
of guiding principles and best practices that I found effective in frontend and
backend development

- Favor modularity, composition and orthogonal design as much as possible
- Abstract out over implementation details and design system in terms of
  components, interfaces and dataflows
- Prefer declarative approach to component definition (e. g. SQL, HTML, other
  DSLs)
- Automate as much as possible (e. g. compilation, testing, deployment,
  monitoring) via CI/CD
- Have simple yet effective tools for testing and benchmarking
- Prefer set of flexible libraries over comprehensive frameworks
- Favor statically typed language specifically for the backend whenever possible
- Finally, some interesting option for developing backend
  [Nim](https://nim-lang.org/) and frontend [Svelte](https://svelte.dev/)
