Authentication system requires a JWS in Authorization header in `Bearer <JWS>` format. Example header:
```
Authorization: Bearer eyJhbGciOiJFZERTQSIsImp3ayI6eyJjcnYiOiJFZDI1NTE5IiwieCI6InFlU3ZrUzZnaW9CcjRlX2I2em8zTWlPT1NYQW90VjkwdDVLajNsMjh5cm8iLCJrdHkiOiJPS1AifX0.eyJwYXRoIjoiL2FwaS9lZHVjYXRvci92MS9zdHVkZW50cyIsInRpbWUiOiIyMDI1LTAxLTAxVDAwOjAwOjAwWiJ9.0_sRvkXTaJTlnLHSjReH70VNFOLx0kdGHDmiDWhUr6H25UCvc5kPD6qn9pDlUwe0uKMpQCGIt_v4hnwWcfVlDA
```

JWS header MUST contain "jwk" parameter ([RFC7515, section 4.1.3](https://tools.ietf.org/html/rfc7515#section-4.1.3)) with a
public JWK corresponding to the signature. JWK MUST have key type "OKP" and curve type "Ed25519", public key parameter "x"
should be encoded via base64Url (accordingly to [RFC8037](https://tools.ietf.org/html/rfc8037)). Example JWK:
```
{ "kty": "OKP", "crv": "Ed25519", "x": "2qbm2mPrVVW_yFsHUMzNt3hLdalGLTN8ucI4e-Cn6fI" }
```
This JWK represents a Disciplina public key of an Educator. The whole JWS should be created via the Educator's secret key.
JWS payload MUST be a JSON object with fields "path" and "time". Field "path" should contain the endpoint URL path and "time"
should contain the time the request has been made in [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601) format. Example JWS
payload:
```
{
  "time": "2025-01-01T00:00:00.000000000Z",
  "path": "/api/educator/v1/students"
}
```
If the request's raw path doesn't match the one in "path" or "time" is more than 5 minutes behind the server current time,
the authentication process will fail.
