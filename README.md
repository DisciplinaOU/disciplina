# Disciplina

Blockchain platform for the field of education.

## Build

This is a standard Stack project, i.e. you can just run `stack build` provided
that you have installed the following native dependencies:

- GMP
- OpenSSL
- pkg-config
- RocksDB
- ZeroMQ
- zlib

### Nix

Install Nix with [NixOS/nix#2409][] patch:

```sh
nix-env -f https://github.com/serokell/nixpkgs/archive/master.tar.gz -iA nix
```

[Nix]: https://nixos.org/nix/
[NixOS/nix#2409]: https://github.com/NixOS/nix/pull/2409

Set up Disciplina binary cache so that you don't have to build dependencies:

```sh
nix-shell -p cachix --run 'sudo cachix use disciplina'
```

For production builds, run `nix-build`.

For incremental builds, run `nix-shell`. Then, use either `stack build` or
`cabal new-build all` as you normally would. This will only build local packages,
all dependencies are managed by Nix.
