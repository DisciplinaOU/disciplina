# Disciplina witness

Main node/witness library and executable.

## Building from the source code

```bash
> stack build disciplina-witness
```

Dependencies:

* [Stack](https://docs.haskellstack.org/en/stable/README/) tool
* [RocksDB](https://github.com/facebook/rocksdb/blob/master/INSTALL.md)

## Running the witness node

```bash
> stack exec dscp-witness -- <parameters>
```

For ready scripts see [scripts section](#scripts).

### Command-line arguments

For the most actual information run

```bash
> stack exec dscp-witness -- --help
```

### Configuration

See [configuration section](/docs/config.md) document.

### Scripts
<a name="scripts"></a>

* [Single node launcher](../scripts/launch/node.hs)

  Example:
  ```bash
  > ./scripts/launch/node.hs witness
  ```

* [Witnesses cluster launcher](../scripts/launch/demo.sh)
  <a name="launch-witness-cluster"></a>

  Example:
  ```bash
  > ./scripts/launch/demo.sh 3
  ```
  for three nodes.
