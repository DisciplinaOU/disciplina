# Configuration

This document describes structure of configuration file.

Configuration is a `.yaml` file containing parameters common for all nodes.
See an example in [config.yaml](/config.yaml) file.

Top-level entries describe a single setting, be it local demo or production cluster.
Yaml-key of such entry is refered as "configuration key". The configuration key used by the node is defined in command line parameters.

Each setting contains following parameters:

* `slotDuration` - single slot duration, in milliseconds.

* `genesis` - describes initial state of the chain.
  * `genesisSeed` - a text seed which affects generation of some minor data.

  * `governance` - specifies way of witness'es addresses generation. Requires `type` field, available values:
    * `committeeClosed` - list of witness addresses is specified explicitely in `participants` field.

      Example:
      ```yaml
      governance:
        type: committeeClosed
        participants:
        - LL4qKqrNmFmSM8C6PRqCVLuVsB1JJrr6PVqdCGuMe1dG9FUmvQbkfEE7
        - LL4qKqMAKAEqPSjme57ad67149x5czYkbvhqgqsntG45xZkBDa563oec
      ```

    * `committeeOpen` - for test purposes, witness'es addresses are automatically derived from given root secret key. `secret` field contains text seed for generating root secret, `n` field contains expected number of witnesses.

      Example:
      ```yaml
      governance:
        type: committeeOpen
        secret: "root_secret"
        n: 4
      ```

  * `distribution` - describes allocation of initial money among accounts. Contains a list, each element of which is one of:
    * `equal: M`, where `M` is amount of money, which would be devided among witnesses evenly.
    * `specific: <pairs>`, where `pairs` is a list of address-money pairs.

      Example:
      ```yaml
      destribution:
        - equal: 1000

        - specific:
          - - LL4qKn62hrdLh1vkyiidEFLfCaAngaVmtu3ipGVSubhackXS4pXNbeDC
            - 100
          - - LL4qKqMAKAEqPSjme57ad67149x5czYkbvhqgqsntG45xZkBDa563oec
            - 100
      ```
