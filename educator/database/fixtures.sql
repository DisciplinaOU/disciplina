
insert into Courses (desc) values ("Math");
insert into Courses (desc) values ("Physics");
insert into Courses (desc) values ("Poetry");

--

insert into Subjects (id, course_id, desc)
    select  1, id, "Algebra"
    from    Courses
    where   desc = "Math";

insert into Subjects (id, course_id, desc)
    select  2, id, "Geometry"
    from    Courses
    where   desc = "Math";

insert into Subjects (id, course_id, desc)
    select  3, id, "Newton"
    from    Courses
    where   desc = "Physics";

insert into Subjects (id, course_id, desc)
    select  4, id, "Lagrange"
    from    Courses
    where   desc = "Physics";

insert into Subjects (id, course_id, desc)
    select  5, id, "Pushkin"
    from    Courses
    where   desc = "Poetry";

insert into Subjects (id, course_id, desc)
    select  6, id, "Lermontov"
    from    Courses
    where   desc = "Poetry";

--

insert into Students (addr) values ("Vasua");
insert into Students (addr) values ("Petua");

--

insert into   StudentCourses (student_addr, course_id)
    select    addr, Courses.id
    from      Students
    left join Courses
    where     desc = "Math"
          and addr = "Vasua";

insert into   StudentCourses (student_addr, course_id)
    select    addr, Courses.id
    from      Students
    left join Courses
    where     desc = "Poetry"
          and addr = "Vasua";

insert into   StudentCourses (student_addr, course_id)
    select    addr, Courses.id
    from      Students
    left join Courses
    where     desc = "Math"
          and addr = "Petua";

insert into   StudentCourses (student_addr, course_id)
    select    addr, Courses.id
    from      Students
    left join Courses
    where     desc = "Physics"
          and addr = "Petua";

insert into   Assignments (hash, course_id, contents_hash, type, desc)
    select    "Add two nums", Courses.id, "2+2=?", 0, "addition"
    from      Courses
    where     Courses.desc = "Math";

insert into   Assignments (hash, course_id, contents_hash, type, desc)
    select    "Solve eq", Courses.id, "pV=?", 0, "gases"
    from      Courses
    where     Courses.desc = "Physics";

insert into   Assignments (hash, course_id, contents_hash, type, desc)
    select    "make poem", Courses.id, "poem=?", 0, "writting"
    from      Courses
    where     Courses.desc = "Poetry";

--

insert into   StudentAssignments (student_addr, assignment_hash)
    select    addr, hash
    from      Students
    left join Assignments
    where     Students.addr = "Petua"
          and Assignments.hash = "Add two nums";

insert into   StudentAssignments (student_addr, assignment_hash)
    select    addr, hash
    from      Students
    left join Assignments
    where     Students.addr = "Vasua"
          and Assignments.hash = "Solve eq";

insert into   StudentAssignments (student_addr, assignment_hash)
    select    addr, hash
    from      Students
    left join Assignments
    where     Students.addr = "Petua"
          and Assignments.hash = "make poem";

--

insert into   Submissions
              ( hash
              , student_addr
              , assignment_hash
              , contents_hash
              , signature
              )
    select    "4", student_addr, assignment_hash, "is 4", "I'm Petua"
    from      StudentAssignments
    where     student_addr = "Petua"
          and assignment_hash = "Add two nums";

insert into   Submissions
              ( hash
              , student_addr
              , assignment_hash
              , contents_hash
              , signature
              )
    select    "nRT", student_addr, assignment_hash, "its gas", "V"
    from      StudentAssignments
    where     student_addr = "Vasua"
          and assignment_hash = "Solve eq";

--

insert into   Transactions
              ( hash
              , submission_hash
              , grade
              , time
              , idx
              )
    select    "1st trans", Submissions.hash, 4, 2018-01-01, 0
    from      Submissions
    where     Submissions.hash = "4";

insert into   Transactions
              ( hash
              , submission_hash
              , grade
              , time
              , idx
              )
    select    "2nd trans", Submissions.hash, 4, 2018-01-01, 0
    from      Submissions
    where     Submissions.hash = "nRT";
