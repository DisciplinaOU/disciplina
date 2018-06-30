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

-- Creating 'Courses' table.
--
create table if not exists Courses (

    -- Sqlite3 will force ascending primary key to be non-null, replacing
    --  null with autoincremented key (while sqlite2 won't).
    --
    id    INTEGER,
    desc  TEXT     null,

    primary key (id asc)
);

-- Creating 'Subjects' table.
--
create table if not exists Subjects (
    id         INTEGER  not null,
    course_id  INTEGER  not null,
    desc       TEXT     null,

    primary key (id, course_id),
    foreign key (course_id) references Courses (id)
);

create index if not exists Subject_course_ids on Subjects (course_id);

-- Creating 'Students' table.
--
create table if not exists Students (
    -- The "primary key" doesn't make field non null in sqlite.
    --
    addr  BLOB  not null,

    primary key (addr)

) without rowid;

-- Creating 'StudentCourses' table.
--
-- [Appears to be] many-to-many ref between Students and Courses.
--
create table if not exists StudentCourses (
    student_addr  BLOB     not null,
    course_id     INTEGER  not null,

    -- Composite primary.
    --
    primary key (student_addr, course_id),

    foreign key (student_addr) references Students (addr),
    foreign key (course_id)    references Courses  (id)

) without rowid;

create index if not exists StudentCourses_student_addr on StudentCourses (student_addr);
create index if not exists StudentCourses_course_id    on StudentCourses (course_id);

-- Creating 'Assignments' table.
--
create table if not exists Assignments (
    hash           BLOB     not null,
    course_id      INTEGER  not null,
    contents_hash  BLOB     not null,
    desc           TEXT     null,

    primary key (hash),
    foreign key (course_id) references Courses(id)

) without rowid;

create index if not exists Assigments_course_id on Assignments (course_id);

-- Creating 'StudentAssignments' table.
--
-- [Appears to be] many-to-many ref between Students and Assigments.
--
create table if not exists StudentAssignments (
    student_addr     BLOB  not null,
    assignment_hash  BLOB  not null,

    primary key (student_addr, assignment_hash),

    foreign key (student_addr)    references Students    (addr)
    foreign key (assignment_hash) references Assignments (hash)

) without rowid;

create index if not exists StudentAssigments_student_addr    on StudentAssignments (student_addr);
create index if not exists StudentAssigments_assignment_hash on StudentAssignments (assignment_hash);

-- Creating 'Submissions' table.
--
create table if not exists Submissions (
    hash             BLOB     not null,
    student_addr     BLOB     not null,
    assignment_hash  BLOB     not null,
    contents_hash    BLOB     not null,
    signature        BLOB     not null,

    primary key (hash),

    foreign key (student_addr)    references Students    (addr)
    foreign key (assignment_hash) references Assignments (hash)

) without rowid;

create index if not exists Submissions_student_addr    on Submissions (student_addr);
create index if not exists Submissions_assignment_hash on Submissions (assignment_hash);

-- Creating 'Transactions' table.
--
create table if not exists Transactions (
    hash             BLOB     not null,
    submission_hash  BLOB     not null,
    grade            INTEGER  not null,
    time             TIME     not null,
    idx              INTEGER  not null,    -- Index inside a block. Can be 0 or -1 for every mempool transaction.

    primary key (hash),
    foreign key (submission_hash) references Submissions(hash)

) without rowid;

create index if not exists Transactions_submission_hash on Transactions (submission_hash);

-- Creating 'Blocks' table.
-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
create table if not exists Blocks (
    idx        INTEGER          ,
    hash       BLOB     not null,
    time       TIME     not null,
    prev_hash  BLOB     null,
    atg_delta  BLOB     not null,
    mroot      BLOB     not null,
    mtree      BLOB     not null,

    primary key (idx)

) without rowid;

create index if not exists Blocks_hash on Blocks (hash);
create index if not exists Blocks_prev_hash on Blocks (prev_hash);

-- Creating 'BlocksTxs' table.
--
create table if not exists BlockTxs (
    blk_idx  INTEGER  not null,
    tx_hash  BLOB     not null,

    primary key (tx_hash),  -- A transaction can belong only to one block

    foreign key (blk_idx) references Blocks      (idx),
    foreign key (tx_hash) references Transactions(hash)

) without rowid;

create index if not exists BlockTxs_blk_idx on BlockTxs (blk_idx);

commit;
