# Test transaction creation

This document describes step by step how to make a test transaction for a controlled cluster of witness nodes.

## Local deployment

1. [Launch a cluster](/witness/README.md#launch-witness-cluster).
2. Take a secret key.
To get a secret with money on it, look in [configuration](/configuration.yaml) file, find genesis destribution (if the demo script for cluster launch was used, then corresponding distribution is described in `demo.core.genesis.destribution` variable; see also [configuration file description](./configuration.md)). This variable should contain several test keys, copy one of secrets in comment.

3. Launch [wallet application](/wallet/README.md), specifying one of witness nodes as backend. Assuming wallet with UI is used, transaction can be made as following:
  * Create a wallet. After selecting `Add wallet` on the left pane, find `Restore wallet from secret key` section, insert your secret key (leaving password field empty) and press `Restore` button.
  * Make a transaction. Select your wallet on the left pane and find `Send transaction` section, insert desired `Address` and `Amount` and press `Send` button.

## Staging

To make it possible to use test transactions in deployed cluster, several keys should be allocated into configuration provisionally. Add addresses to `<setting-name>.core.genesis.distribution.specific` list and save corresponding secrets. A pack of secrets can be generated via faucet or in `ghci`:
```
stack ghci --ghci-options "-hide-package base" disciplina-core
> secrets <- withSeed "mykeys" $ replicateM 10 genSecretKey
> let addresses = map (mkAddr . toPublic) secrets 
```
