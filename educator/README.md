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
  To launch educator with bot to automatically react on student's actions:

  ```bash
  > ./scripts/launch/node.sh educator bot
  ```

## Running educator tests

### Tuning Postgres server

As soon as tests involve creation of one database per test case, you will need to connect
to the server as a superuser there.

This can be achieved in the following way (read the entire section before doing anything).

First create a postgres user:
``` bash
sudo -u postgres createuser --interactive --pwprompt
```

You will be asked for a user name, password and required privilegies.

When launching tests, specify your credentials:
* Set environmental variable `PGUSER=username`, by default your OS username is used;
* If you specified a password for your user, add it to
[`.pgpass` file](https://www.postgresql.org/docs/9.1/libpq-pgpass.html).
