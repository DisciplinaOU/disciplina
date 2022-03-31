# Disciplina

[![Build status](https://badge.buildkite.com/e7979d0942ce6a3cf7c9cea9a9e23915789f0baeed89ee14f3.svg?branch=master)](https://buildkite.com/disciplina/disciplina)

Blockchain platform for the field of education.

## Build

This is a standard Stack project, i.e. you can just run `stack build` provided
that you have installed the following native dependencies:

- GMP
- OpenSSL
- pkg-config
- Postgres
- RocksDB
- ZeroMQ
- zlib

However, it is advisable that you use Nix in conjunction with Stack for incremental builds (see below).

### Nix

The project uses [Nix](https://nixos.org/nix/) with IOHK's haskell.nix library for builds and dependency management.

Steps to obtain a working development environment:

- Install Nix as per the [guide](https://nixos.wiki/wiki/Nix_Installation_Guide)
- Set up the IOHK binary cache as per the [instruction](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html)
- Enter the Nix shell: `$ nix-shell`. (This will take a lot of time, because all the dependencies are being fetched and built)
- Use the `stack build` command to build the project.


#### Running multi-educator executable

Inside the Nix shell, you can start the multi-educator backend by using the command:

```
$ stack exec dscp-multi-educator --config config.yaml --config-key new-web3
```

#### Running tests

Tests in the `disciplina-educator` package which involve PostgreSQL queries require temporary DB setup using `pg_tmp`.
There is a script `./scripts/test/educator.sh` which does exactly that.
