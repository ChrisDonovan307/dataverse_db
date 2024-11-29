-- 2. SQL: Nested Views --------------------------------------------------

-- a
create view Proj_assignment as
select p.Pname, e.Fname, e.Lname, d.Dname, w.Hours
from EMPLOYEE e, DEPARTMENT d, PROJECT p, WORKS_ON w
where e.Dno = d.Dnumber
and p.Dnum = d.Dnumber
and e.Ssn = w.Essn
and w.Pno = p.Pnumber
order by p.Pname;

select * from Proj_assignment;


-- b
create view ResProj_assignment as
select Pname, Fname, Lname, Hours
from Proj_assignment
where Dname = 'Research';

select * from ResProj_assignment;

-- c
create view ResProj_assignment_summary as
select Pname, count(*) as num_employees, avg(Hours) as avg_hours
from ResProj_assignment
group by Pname;

select * from ResProj_assignment_summary;

-- d
select Pname, num_employees, avg_hours
from ResProj_assignment_summary
where num_employees > 2;


-- e
select p.Pname, count(distinct e.Ssn) as num_employees, avg(e.Salary) as avg_salary
from EMPLOYEE e, DEPARTMENT d, PROJECT p, WORKS_ON w
where e.Dno = d.Dnumber
and p.Dnum = d.Dnumber
and e.Ssn = w.Essn
and w.Pno = p.Pnumber
and d.Dname = 'Research'
group by p.Pname
having count(distinct e.Ssn) > 2;


-- f
drop view Proj_assignment;
drop view ResProj_assignment;
drop view ResProj_assignment_summary;
