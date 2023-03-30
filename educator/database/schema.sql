-- Postgres syntax.

-- As sql keywords are composite and have spaces inside them,
-- I used 2 spaces to separate names, types and constraints visually.
--
-- Also, postgres is said to create indices on PKs automatically, so I will omit them.

----------------------------------------------------------------------------
-- General info
----------------------------------------------------------------------------

-- Creating 'transactions' table.
--
create table if not exists transactions (
    hash             BYTEA        not null,
    creation_time    TIMESTAMPTZ  not null,
    idx              INTEGER      not null,    -- Index inside a block. -1 for every mempool transaction.
    entity           INTEGER      not null,    -- Entity which transaction refers to
    data             JSONB        not null,    -- Transaction data

    primary key (hash)
);

create index if not exists transactions_entity on transactions (entity);

-- Creating 'blocks' table.
-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
create table if not exists blocks (
    idx           INTEGER      not null,
    hash          BYTEA        not null,
    creation_time TIMESTAMPTZ  not null,
    prev_hash     BYTEA        null,
    pub_tx_id     BYTEA        null,
    merkle_root   BYTEA        not null,
    merkle_tree   BYTEA        not null,

    primary key (idx)

);

create index if not exists blocks_hash on blocks (hash);
create index if not exists blocks_prev_hash on blocks (prev_hash);

-- Creating 'blocks_txs' table.
--
create table if not exists block_txs (
    __idx   INTEGER  not null,
    __hash  BYTEA    not null,

    primary key (__hash),  -- A transaction can belong only to one block

    foreign key (__idx)  references blocks      (idx),
    foreign key (__hash) references transactions(hash)

);

create index if not exists block_txs_blk_idx on block_txs (__idx);

----------------------------------------------------------------------------
-- Certificates
----------------------------------------------------------------------------

-- Creating 'certificates' table.
--
create table if not exists certificates (
    hash  BYTEA  not null,
    meta  JSONB  not null,
    pdf   BYTEA  not null,

    primary key (hash)

);

-- Tying together blocks and certificates
--
create table if not exists certificate_blocks (
    __idx   INTEGER  not null,
    __hash  BYTEA    not null,

    primary key (__idx, __hash),
    -- Certificate can potentially contain many blocks, and one block can
    -- potentially be included in several certificates.

    foreign key (__idx)  references blocks       (idx),
    foreign key (__hash) references certificates (hash)
);

create index if not exists certificate_blocks_blk_idx  on certificate_blocks (__idx);
create index if not exists certificate_blocks_cert_idx on certificate_blocks (__hash);

-- Creating 'certificates_version' table.
--
create table if not exists certificates_version (
    id    CHAR(4)   not null,
    item  SMALLINT  not null,

    primary key (id)

);
