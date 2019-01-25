-- Postgres syntax.

-- As sql keywords are composite and have spaces inside them,
-- I used 2 spaces to separate names, types and constraints visually.
--
-- Also, postgres is said to create indices on PKs automatically, so I will omit them.

----------------------------------------------------------------------------
-- General info
----------------------------------------------------------------------------

-- Creating 'courses' table.
--
create table if not exists courses (

    -- Sqlite3 will force ascending primary key to be non-null, replacing
    --  null with autoincremented key (while sqlite2 won't).
    --
    id     BIGSERIAL  PRIMARY KEY,
    "desc" TEXT       not null
);

-- Creating 'subjects' table.
--
create table if not exists subjects (
    id          BIGSERIAL  PRIMARY KEY,
    course__id  BIGINT     not null,
    "desc"      TEXT       not null,

    foreign key (course__id) references courses (id)
);

create index if not exists subject_course_ids on subjects (course__id);

-- Creating 'students' table.
--
create table if not exists students (
    -- The "primary key" doesn't make field non null in sqlite.
    --
    addr  BYTEA  not null,

    primary key (addr)

);

-- Creating 'student_courses' table.
--
-- [Appears to be] many-to-many ref between students and courses.
--
create table if not exists student_courses (
    __addr  BYTEA    not null,
    __id    BIGINT   not null,

    -- Composite primary.
    --
    primary key (__addr, __id),

    foreign key (__addr) references students (addr),
    foreign key (__id)   references courses  (id)

);

create index if not exists student_courses_student_addr on student_courses (__addr);
create index if not exists student_courses_course_id    on student_courses (__id);

-- Creating 'assignments' table.
--
create table if not exists assignments (
    hash           BYTEA    not null,
    course__id     BIGINT   not null,
    contents_hash  BYTEA    not null,
    type           INTEGER  not null,
    "desc"         TEXT     not null,

    primary key (hash),
    foreign key (course__id) references courses(id)

);

create index if not exists assigments_course_id on assignments (course__id);

-- Creating 'student_assignments' table.
--
-- [Appears to be] many-to-many ref between students and Assigments.
--
create table if not exists student_assignments (
    __addr  BYTEA  not null,
    __hash  BYTEA  not null,

    primary key (__addr, __hash),

    foreign key (__addr) references students    (addr),
    foreign key (__hash) references assignments (hash)

);

create index if not exists student_assigments_student_addr    on student_assignments (__addr);
create index if not exists student_assigments_assignment_hash on student_assignments (__hash);

-- Creating 'submissions' table.
--
create table if not exists submissions (
    hash             BYTEA       not null,
    student__addr    BYTEA       not null,
    assignment__hash BYTEA       not null,
    contents_hash    BYTEA       not null,
    signature        BYTEA       not null,
    creation_time    TIMESTAMPTZ not null,

    primary key (hash),

    foreign key (student__addr)    references students    (addr),
    foreign key (assignment__hash) references assignments (hash)

);

create index if not exists submissions_student_addr    on submissions (student__addr);
create index if not exists submissions_assignment_hash on submissions (assignment__hash);

-- Creating 'transactions' table.
--
create table if not exists transactions (
    hash             BYTEA        not null,
    submission__hash BYTEA        not null,
    grade            SMALLINT     not null,
    creation_time    TIMESTAMPTZ  not null,
    idx              INTEGER      not null,    -- Index inside a block. -1 for every mempool transaction.

    primary key (hash),
    foreign key (submission__hash) references submissions(hash) on delete restrict

);

create index if not exists transactions_submission_hash on transactions (submission__hash);

-- Creating 'blocks' table.
-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
create table if not exists blocks (
    idx           INTEGER      not null,
    hash          BYTEA        not null,
    creation_time TIMESTAMPTZ  not null,
    prev_hash     BYTEA        null,
    atg_delta     BYTEA        not null,
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

create index if not exists certificates_student_name
    on certificates ((meta ->> 'studentName'));
    -- Double parenthesis ^ are required because of the non trivial expression.

create index if not exists certificates_student_name
    on certificates ((meta ->> 'number'));

-- Creating 'certificates_version' table.
--
create table if not exists certificates_version (
    id    CHAR(4)   not null,
    item  SMALLINT  not null,

    primary key (id)

);
