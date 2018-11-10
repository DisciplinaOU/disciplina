-- Valid for squlite3 3.10+ (if not exists, without rowid).

-- As sql keywords are composite and have spaces inside them,
-- I used 2 spaces to separate names, types and constraints visually.
--
-- Also, sqlite is said to create indices on PKs automatically, so I will omit them.
--
-- As the sqlite will create additional `rowid` unique PK even if another
--  non integer or integer desc PK is present, some of tables are created
--  WITHOUT ROWID.

begin transaction;

-- Creating 'courses' table.
--
create table if not exists courses (

    -- Sqlite3 will force ascending primary key to be non-null, replacing
    --  null with autoincremented key (while sqlite2 won't).
    --
    id    INTEGER    PRIMARY KEY AUTOINCREMENT,
    desc  TEXT     not null
);

-- Creating 'subjects' table.
--
create table if not exists subjects (
    id          INTEGER  not null,
    course__id  INTEGER  not null,
    desc        TEXT     not null,

    primary key (id, course__id),
    foreign key (course__id) references courses (id)
);

create index if not exists subject_course_ids on subjects (course__id);

-- Creating 'students' table.
--
create table if not exists students (
    -- The "primary key" doesn't make field non null in sqlite.
    --
    addr  BLOB  not null,

    primary key (addr)

) without rowid;

-- Creating 'student_courses' table.
--
-- [Appears to be] many-to-many ref between students and courses.
--
create table if not exists student_courses (
    __addr  BLOB     not null,
    __id    INTEGER  not null,

    -- Composite primary.
    --
    primary key (__addr, __id),

    foreign key (__addr) references students (addr),
    foreign key (__id)   references courses  (id)

) without rowid;

create index if not exists student_courses_student_addr on student_courses (__addr);
create index if not exists student_courses_course_id    on student_courses (__id);

-- Creating 'assignments' table.
--
create table if not exists assignments (
    hash           BLOB     not null,
    course__id     INTEGER  not null,
    contents_hash  BLOB     not null,
    type           INTEGER  not null,
    desc           TEXT     not null,

    primary key (hash),
    foreign key (course__id) references courses(id)

) without rowid;

create index if not exists assigments_course_id on assignments (course__id);

-- Creating 'student_assignments' table.
--
-- [Appears to be] many-to-many ref between students and Assigments.
--
create table if not exists student_assignments (
    __addr     BLOB  not null,
    __hash  BLOB  not null,

    primary key (__addr, __hash),

    foreign key (__addr) references students    (addr)
    foreign key (__hash) references assignments (hash)

) without rowid;

create index if not exists student_assigments_student_addr    on student_assignments (__addr);
create index if not exists student_assigments_assignment_hash on student_assignments (__hash);

-- Creating 'submissions' table.
--
create table if not exists submissions (
    hash             BLOB      not null,
    student__addr    BLOB      not null,
    assignment__hash BLOB      not null,
    contents_hash    BLOB      not null,
    signature        BLOB      not null,
    creation_time    TIMESTAMP not null,

    primary key (hash),

    foreign key (student__addr)    references students    (addr)
    foreign key (assignment__hash) references assignments (hash)

) without rowid;

create index if not exists submissions_student_addr    on submissions (student__addr);
create index if not exists submissions_assignment_hash on submissions (assignment__hash);

-- Creating 'transactions' table.
--
create table if not exists transactions (
    hash             BLOB      not null,
    submission__hash BLOB      not null,
    grade            INTEGER   not null,
    creation_time    TIMESTAMP not null,
    idx              INTEGER   not null,    -- Index inside a block. -1 for every mempool transaction.

    primary key (hash),
    foreign key (submission__hash) references submissions(hash) on delete restrict

) without rowid;

create index if not exists transactions_submission_hash on transactions (submission__hash);

-- Creating 'blocks' table.
-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
create table if not exists blocks (
    idx           INTEGER          ,
    hash          BLOB      not null,
    creation_time TIMESTAMP not null,
    prev_hash     BLOB      null,
    atg_delta     BLOB      not null,
    merkle_root   BLOB      not null,
    merkle_tree   BLOB      not null,

    primary key (idx)

) without rowid;

create index if not exists blocks_hash on blocks (hash);
create index if not exists blocks_prev_hash on blocks (prev_hash);

-- Creating 'blocks_txs' table.
--
create table if not exists block_txs (
    __idx  INTEGER  not null,
    __hash  BLOB    not null,

    primary key (__hash),  -- A transaction can belong only to one block

    foreign key (__idx) references  blocks      (idx),
    foreign key (__hash) references transactions(hash)

) without rowid;

create index if not exists block_txs_blk_idx on block_txs (__idx);

commit;
