# Disciplina educator library

Disciplina educator lib and executable.

## Building from the source code

```bash
> stack build disciplina-educator
```

Dependencies:

* [Stack](https://docs.haskellstack.org/en/stable/README/) tool
* [RocksDB](https://github.com/facebook/rocksdb/blob/master/INSTALL.md)
* [PostgreSQL](https://www.postgresql.org/)

## Running the educator node

```bash
> stack exec dscp-educator -- <parameters>
```

For ready scripts see [scripts section](#scripts).

Note that educator node is a full witness node as well, thus it requires all the
same parameters witness does.

### Command-line arguments

For the most actual information run

```bash
> stack exec dscp-educator -- --help
```

### Configuration

See [configuration section](/docs/config.md) document.

### Scripts
<a name="scripts"></a>

* [Single node launcher](../scripts/launch/node.sh)

  Example:
  ```bash
  > ./scripts/launch/node.sh educator
  ```

  **Note:** you need Postgres server to be set up, see 'Prerequisites' section of script above first.

  To launch educator with bot to automatically react on student's actions:

  ```bash
  > ./scripts/launch/node.sh educator bot
  ```

## Running educator tests

You will need to run Postgres server in order for `stack exec disciplina-educator` to work.

Or run the [test script](/scripts/test/educator.sh) which does that for you.
**Nix users** need to have `nix: pure: false` in their stack config in order to use this script.

Server still requires a bit of general tunning, if you have just installed your Postgres server
read [`educator.sh`'s](/scripts/test/educator.sh) 'Prerequisites' section first.
