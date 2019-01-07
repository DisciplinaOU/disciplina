# Disciplina

[![Build status](https://badge.buildkite.com/e7979d0942ce6a3cf7c9cea9a9e23915789f0baeed89ee14f3.svg?branch=master)](https://buildkite.com/disciplina/disciplina)

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

Set up Disciplina binary cache so that you don't have to build dependencies:

```sh
sudo $(nix-build pkgs.nix -A cachix --no-out-link)/bin/cachix use disciplina
```

If you are on a single-user Nix install (`nix-shell -p nix-info --run nix-info`
should say `multi-user?: no`), omit `sudo` in the command above. You will see
`No permission` errors, that's fine.

If you are on NixOS, make sure to add `https://cache.nixos.org` to `nix.binaryCaches`,
otherwise main Nix binary cache stops working. See [cachix/cachix#128][].

[cachix/cachix#128]: https://github.com/cachix/cachix/pull/128

Next, install Nix with [NixOS/nix#2409][] patch:

```sh
nix-env -f pkgs.nix -iA nix
```

[Nix]: https://nixos.org/nix/
[NixOS/nix#2409]: https://github.com/NixOS/nix/pull/2409

If you test Disciplina or develop software that integrates with it, run
`nix-env -f release.nix -iA disciplina`. This will install Disciplina packages
into your user profile. Then you can use `scripts/launch/node.hs` to start nodes.

For production builds, run `nix-build`.

For incremental builds, run `nix-shell`. Then, use either `stack build` or
`cabal new-build all` as you normally would. This will only build local packages,
all dependencies are managed by Nix.

If you prefer to let Stack handle Haskell dependencies instead of Nix, or if
the above doesn't work for you for whatever reason, leave `nix-shell` and build
with `stack build --nix`. In that case, Nix will only provide native deps.

## Issue tracker

We use [YouTrack](https://issues.serokell.io/issues/DSCP) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.
