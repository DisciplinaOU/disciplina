# Disciplina-tools

Tools for disciplina.

### Installation

You may find it useful to once do

```bash
stack install disciplina-tools
```

## txperf

An utility to flood witness node with valid transactions.

Run as follows:

```bash
dscp-txperf --config txperf.json
```

Contents of the config file must be in the following format:

```json
{
  "witness": "witness-1.disciplina.serokell.team",
  "accKeys": [
    "zD9OLyp86WwV9b/3HuSuxY9Q9FMB38ik/uGnIS0p/cU=",
    "W+DwF8TXAxy9vgZvcHK9/qIwcz1S4QIZLWyEB3ggEkc="
  ],
  "genAccs": 2,
  "txCount": 20,
  "txAsync": true
}
```

`genAccs` accounts will be generated, and then `txCount` random 1-coin transactions will be sent between the listed and generated accounts.

`txAsync` defines, whether `/tx` or `/tx/async` endpoint is used.

Configuration file suitable for `./scripts/launch/node w` is already present in this directory for convenience.

## keygen

This is a small tool to manipulate with secret key. Along with generating new one, it allows to produce various deriviative data.

### Usage

Main usage pattern is
```bash
echo <input with secret> | dscp-keygen <input-type-option> --command <command>
```

Input defines which secret key will be used as base for further operations.
It can be specified directly, as encrypted key with password, as path to keyfile or be generated from scratch, depending on `input-type-option` option.

Execute `dscp-keygen --help` for the list of available 'input-type' options.

Command stands for data to be derived from the base secret key. Commands have options (not mandatory), which vary how data will be displayed.

List of options types:

* View - determines how to display raw data, can be `raw`, `base64` or `hex`.
* Prettiness - for large outputs, whether to use pretty multiline output.
  Can be `pretty` or `one-line`
* Password - password used to generate secret key.

Following commands are supported:

* `secret[:<view=base64>]` - display secret itself.
* `public[:<view=hex>]` - display public key.
* `address` - address, base58bitcoin.
* `esecret[:<password="">[:<view=hex>]]` - display encrypted secret key.
* `keyfile[:<password="">[:<prettiness=pretty>]]` - display keyfile.
* `educator-auth:endpointName` - produce JWT token for educator node from given educator secret key and name of accessed endpoint.
* `student-submission[:<seed="">]` - produce JSON request for `POST /api/student/v1/submissions` endpoint.

### Examples

_Reminder: no option after `:` is mandatory._

* Generate secret key:

```bash
echo 1000 | dscp-keygen --seed --command secret
```

* Generate truly random secret:

```bash
cat /dev/urandom | head -c 32 | dscp-keygen --seed --command secret
```

* Generate key file with password:

```bash
echo 1000 | dscp-keygen --seed --command keyfile:password > secret.key
chmod 660 secret.key
```

* Read key file:

```base
cat secret.key | dscp-keygen --keyfile --password qwerty123 --command secret
```

* Produce public key:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret  --command "public:hex"
```

* Produce address:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret --command "address"
```

* Encrypt secret key:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret --command "esecret:12345678"
```

* Produce JWT token to authenticate in Educator or Student API:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret --command "educator-auth:/api/student/v1/courses"
```
You can later append token with
1. `-H "Authorization: Bearer <token>"` for `curl`
2. `Authorization:Bearer\ <token>` for [`http`](https://httpie.org/).

* Submit a new submission from student - owner of given secret key:

```base
echo 456 \
    | dscp-keygen --seed --command student-submission:3656234 \
    | http POST :8090/api/student/v1/submissions
```
(_Reminder: option of `student-submission` command is seed which allows to generate unique submissions._)

See [example of use](/scripts/test/student-submissions-spam.sh).

For **testing purposes** you will most probably want to always run `dscp-keygen --seed` and vary command only.
