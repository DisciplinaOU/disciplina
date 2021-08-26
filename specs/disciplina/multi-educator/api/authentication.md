Authentication system requires a JWS in Authorization header in `Bearer <JWS>` format. Example header:
```
Authorization: Bearer eyJhbGciOiJFZERTQSIsImp3ayI6eyJjcnYiOiJFZDI1NTE5Iiwia3R5IjoiT0tQIiwieCI6IjJnU055MndLU2FJNFl0R1plX0VheHNkdl9CTENmaTVra1Q5eHZ4dF9PMGsifSwidHlwIjoiSldUIn0.eyJkYXRhIjp7ImVtYWlsIjoiZ2VvcmdlLndAc2hhcmtsYXNlcnMuY29tIiwiaWQiOiIwNTg5ZTU3YS05NGZmLTRjMzQtYmUyMi1jNTBiMGY1NGQxODQifSwiZXhwIjoxNTUxMjc5OTE0fQ.4ZKHlaqtQiNCZMOMPQTFToEP89Zq_nRq9dOSWpDGuooKlwtkWdqIJsPzBXfFkEEgDUGJ354QLt8vK0OnMKeKBw
```

JWS header MUST contain "jwk" parameter ([RFC7515, section 4.1.3](https://tools.ietf.org/html/rfc7515#section-4.1.3)) with a
public JWK corresponding to the signature. JWK MUST have key type "OKP" and curve type "Ed25519", public key parameter "x"
should be encoded via base64Url (accordingly to [RFC8037](https://tools.ietf.org/html/rfc8037)). Example JWK:
```
{ "kty": "OKP", "crv": "Ed25519", "x": "2gSNy2wKSaI4YtGZe_Eaxsdv_BLCfi5kkT9xvxt_O0k" }
```
This JWK represents a Disciplina Multi-Educator Login. The whole JWS should be obtained via the Disciplina AAA Microservice.
JWS payload MUST be a JSON object with fields "data" and "exp".
The field "data" should contain the field "id" (that will be used for Educator authentication) and "exp"
should contain the time the before which the JTW is valid in [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601) format. Example JWS
payload:
```
{
  "data": {
    "id": "0589e57a-94ff-4c34-be22-c50b0f54d184"
  },
  "exp": 1551279914
}
```
If "exp" is more than 5 minutes behind the server current time,
the authentication process will fail.
