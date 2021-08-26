# API types documentation

This document describes the format of objects used in various APIs across the project.
For other details, see swagger documentation of [educator][educator-swagger], [student][student-swagger] and [witness][witness-swagger] API.

## Common objects

The following types are always represented in the same format:

| Type       | Representation                | Example                                                            |
| ----       | :--------------:              | :-------:                                                          |
| Address    | base58 with bitcoin alphabet  | `LL4qKrxczNJSehG81SN5dH9boaLNopasSSNaz4a7CKdULH7G7itzcV3j`         |
| Public key | hex                           | `883051192892231ab2fb44e8c023787bc8c41558e44d8c8ef3744859c5e1cf14` |
| Hash       | blake2b (256 bit) hash in hex | `aad78a13b50a014a24633c7d44fd8f8d18f67bbb3fa9cbcedf834ac899759dcd` |

## Student API

### Making a submission

Example of submission request to `POST /submissions` endpoint (using [httpie][httpie]):

```bash
http -v http://localhost:8090/api/student/v1/submissions \
    Authorization:Bearer\ eyJhbGciOiJFZERTQSJ9.eyJkYXQiOnsiYWRQYXRoIjoiL2FwaS9zdHVkZW50L3YxL3N1Ym1pc3Npb25zIiwiYWRUaW1lIjoiMjAyNS0wOC0xMFQxMzoxNTo0MC40NjE5OTgxMzZaIn19.N2pXrr6k9FlFXCaCU7fND1_93vqj52OmB0_9tqLF6Jxf0R-l0nOADBNHqYphdMAkb-u7F07lNEPigAUO0pcXDg \
    contentsHash=01a7cc7de66f6e398ccc5bfdeb2db81646aed6e43f90f7333d9a06ba205b7f74 \
    assignmentHash=219bfb1e71af4f888fce709900862b4467f45b42b0603a56e9428cb3e8eb8940 \
    witness=83005820836d6a5908166c398db194e7af916827f63da021a6a3a1c3edf9ced6608adf2b584007f60245c81f8ba9abf85e24c67cd006ad68c43f8c909a1dc84cc09222937ffde1a9785e2cb0ccfacf582f73f88426fbfb0393c8dc9136bc1b5861a65adca005
```
You can try it on launched [educator node with bot](/educator/README.md#scripts).

In detail, request for this endpoint is formed as follows:

```javascript
function makeSignedSubmission(
    studentSecretKey, // student's secret key
    contentsHash      = "7b6d0c6de38639cc6063e9c36f9dcdb71fff60fe551ccc757246a3bf2fa00f37",
                      // hash of submitted solution
    assignmentHash    // assignment identifier, returned along with other information about an assignment
) {
    return
        { contentsHash: contentsHash
        , assignmentHash: assignmentHash
        , witness: makeWitness(studentSecretKey, contentsHash, assignmentHash)
        }
}
```

If the submission does not contain a solution, i.e. submission is [offline][educator-spec] one, then for `contentsHash` parameter default value should be used.

`Witness` field validates the fact that submission was made by its claimed author and is constructed as in following pseudo-code.

```javascript
function makeWitness(
    studentSecretKey, // student's secret key
    contentsHash,     // hash of submitted solution
    assignmentHash    // assignment identifier, returned along with other information about an assignment
) {
    // Used libraries
    var cbor;     // encodes entities according to CBOR specification, see also ??? (TODO: DSCP-267)
    var ed22519;  // operations on Ed22519 keys, for instance `nacl.sign.keyPair`
    var crc32;    // CRC32 hashsum evaluation

    // Algorithm
    var studentPublicKey = ed22519.toPublic(studentSecretKey)
    var studentAddress = cbor.encode(blake2b(studentPublicKey))
    var submission = cbor.encode([
        0,
        [studentAddress, crc.buf(studentAddress)],
        contentsHash,
        assignmentHash
    ])
    var submissionHash = blake2b(submission)
    var signature = ed22519.sign(studentSecretKey, submissionHash)
    var witness = cbor.serialise([0, studentPublicKey, signature])
    return toHex(witness)
}
```

#### Evaluation example

Let's assume we have following data (all entities are given in hex for convenience, while algorithm itself operates with raw byte arrays):

```javascript
var studentSecretKey = "6dbb1eeeb3e8e81547f307ba9344dae6286a9319458e9596f2ffc43670d5c648"
var contentsHash = "01a7cc7de66f6e398ccc5bfdeb2db81646aed6e43f90f7333d9a06ba205b7f74"
var assignmentHash = "219bfb1e71af4f888fce709900862b4467f45b42b0603a56e9428cb3e8eb8940"
```

Then the algorithm proceeds as follows:

```javascript
studentPublicKey = "836d6a5908166c398db194e7af916827f63da021a6a3a1c3edf9ced6608adf2b"
studentAddress = "97341b982de13dcf99d57bf12150ed433200fdef3a5140fd74267a9ae4748a72"
submission = "8400825822582097341b982de13dcf99d57bf12150ed433200fdef3a5140fd74267a9ae4748a721a7b637e56582001a7cc7de66f6e398ccc5bfdeb2db81646aed6e43f90f7333d9a06ba205b7f745820219bfb1e71af4f888fce709900862b4467f45b42b0603a56e9428cb3e8eb8940"
submissionHash = "dde06e57371fb3ef9e8fafc2b666411112198c5104c0ac6e69f714a65ed7ba0a"
signature = "07f60245c81f8ba9abf85e24c67cd006ad68c43f8c909a1dc84cc09222937ffde1a9785e2cb0ccfacf582f73f88426fbfb0393c8dc9136bc1b5861a65adca005"
witness = "83005820836d6a5908166c398db194e7af916827f63da021a6a3a1c3edf9ced6608adf2b584007f60245c81f8ba9abf85e24c67cd006ad68c43f8c909a1dc84cc09222937ffde1a9785e2cb0ccfacf582f73f88426fbfb0393c8dc9136bc1b5861a65adca005"
```

[httpie]: https://httpie.org/
[educator-spec]: /specs/disciplina/educator/api/spec.org
[educator-swagger]: /specs/disciplina/educator/api/educator.yaml
[student-swagger]: /specs/disciplina/educator/api/student.yaml
[witness-swagger]: /specs/disciplina/witness/api/witness.yaml
