# Authenticaion in Educator and Student API's

This document describes how can backend developers survive in presense of our current autentification. For details on what is it like and how to work with it see swagger documentation of [educator](/specs/disciplina/educator/api/educator.yaml) or [student](/specs/disciplina/educator/api/student.yaml) API.

## Submitting request with `curl`

In order to make a request you need to pass authentication header. To construct one, you can use `makeAuthHeader` function via ghci.
(See also [how to run ghci in ~the wild~ disciplina project](/docs/build-with-ghci.md)).

JTW token produced by this function can later be used in HTTP header.

Step-by-step example:

```bash
stack ghci disciplina-educator
> import Dscp.Util.Test
> secret = detGen 123 arbitrary
> makeAuthHeader secret "/api/educator/v1/students"
"eyJhbGciOiJFZERTQSJ9.eyJkYXQiOnsiYWRQYXRoIjoiL2FwaS9lZHVjYXRvci92MS9zdHVkZW50cyIsImFkVGltZSI6IjIwMjUtMDgtMTBUMTM6MTU6NDAuNDYxOTk4MTM2WiJ9fQ.xqn65GQNSlEktlwy0nPlEJRre0BzEVbAeU69U4yn0jozjhmZQpWmWoojnrhrxWO5FENrNzMu0oj1ujuTWyJhCQ"
```

Then execute your `curl` request with appended `-H "Authorization: Bearer eyJhbGciOiJFZERTQSJ9.eyJkYXQiOnsiYWRQYXRoIjoiL2FwaS9lZHVjYXRvci92MS9zdHVkZW50cyIsImFkVGltZSI6IjIwMjUtMDgtMTBUMTM6MTU6NDAuNDYxOTk4MTM2WiJ9fQ.xqn65GQNSlEktlwy0nPlEJRre0BzEVbAeU69U4yn0jozjhmZQpWmWoojnrhrxWO5FENrNzMu0oj1ujuTWyJhCQ"`.
