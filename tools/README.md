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
echo <input with secret> | dscp-keygen <input-type-option> <command> <command-arguments>
```

Input defines which secret key will be used as a base for further operations.
It can be specified directly, as encrypted key with a password, as a path to the keyfile or be generated from scratch, depending on `input-type-option` option.

Execute `dscp-keygen --help` for the list of available 'input-type' options and to see available commands.

Each command stands for data to be derived from the base secret key. For the description and list of arguments for each command execute `dscp-keygen <command> --help`.

Note, that name of the command argument sometimes collides with options related to the input (e.g. `--password` may relate to a secret on input or to a secret on output).
In such cases, all the options before the command name are considered related to the input, and options after the command name - to the output.

### Examples

* Generate secret key:

```bash
echo 1000 | dscp-keygen --seed secret
```

* Generate truly random secret:

```bash
cat /dev/urandom | head -c 32 | dscp-keygen --seed secret
```

* Generate key file with password:

```bash
echo 1000 | dscp-keygen --seed keyfile --password=qwerty123 > secret.key
chmod 660 secret.key
```

* Read key file:

```base
cat secret.key | dscp-keygen --keyfile --password qwerty123 secret
```

* Produce public key:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret public --hex
```

* Produce address:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret address
```

* Encrypt secret key:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret esecret --password=12345678
```

* Produce JWT token to authenticate in Educator or Student API:

```base
echo '9JsCkzFH/W7SbgvgokeJTsckJs/uoAPWWwC40Y6UfFo=' | dscp-keygen --secret educator-auth /api/student/v1/courses
```
You can later append token with
1. `-H "Authorization: Bearer <token>"` for `curl`
2. `Authorization:Bearer\ <token>` for [`http`](https://httpie.org/).

* Submit a new submission from student - owner of given secret key:

```base
echo 456 \
    | dscp-keygen --seed student-submission --sub-seed=3656234 \
    | http POST :8090/api/student/v1/submissions
```

See [example of use](/scripts/test/student-submissions-spam.sh).

For **testing purposes** you will most probably want to always run `dscp-keygen --seed` and vary command only.
