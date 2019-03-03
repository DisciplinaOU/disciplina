# Running `ghci`

This document describes possible issues arising when running ghci for the Disciplina project.

## Simple case

If you are lucky enough, plain

```bash
> stack ghci disciplina-core
```

or

```bash
> stack repl disciplina-core
```

will work.

## Known issues

#### Duplicated modules error

```
Error: Multiple files use the same module name:
       * Glue found at the following paths
         * /home/martoon/serokell/disciplina/wallet/exec-cli/Glue.hs (disciplina-wallet:exe:dscp-wallet-cli)
         * /home/martoon/serokell/disciplina/wallet/exec/Glue.hs (disciplina-wallet:exe:dscp-wallet)
```

Currently running wallet subproject with `ghci` is not supported. It means that plain `stack ghci` will not work too.

*Solution:* Specify exact subproject name.

#### `base` package conficts with `base-noprelude`

This project relies on `lootbox` repository which depends on `base-noprelude` package, sometimes it may conflict with other packages depending on `base`. Probably the issue arises only on old versions of cabal/stack.

*Solution:* pass `--ghci-options "-hide-package base"` option.

#### `./specs` is not a directory

```
gcc: error: ./specs: Is a directory
`gcc' failed in phase `gcc'. (Exit code: 1)
```

*Solution:* this discussion has helped me (@martoon): https://github.com/raphlinus/pulldown-cmark/issues/122

## Contributions

If you experience an issue not listed here, or know better to solution to existing ones, you are welcome to extend this document.
