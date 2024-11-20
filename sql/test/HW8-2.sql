-- 2. Select statements --------------------------------------------------

-- a
select e.Fname, e.Lname
from EMPLOYEE e
join EMPLOYEE s on e.Super_ssn = s.Ssn
where s.Fname = 'Franklin'
      and s.Lname = 'Wong';

-- b
select p.Pname, sum(w.Hours) as total_hours
from PROJECT p
join WORKS_ON w on p.Pnumber = w.Pno
group by p.Pname;

-- c
insert into WORKS_ON values (333445555, 1, NULL);
insert into WORKS_ON values (333445555, 30, NULL);

select e.Fname, e.Lname, p.Pname
from EMPLOYEE e
join WORKS_ON w on e.Ssn = w.Essn
join PROJECT p on w.Pno = p.Pnumber
order by p.Pname;

rollback;

-- d
delete from WORKS_ON
where Essn = 888665555
      and pNo = 20;

select e.Fname, e.Lname
from EMPLOYEE e
left join WORKS_ON w on e.Ssn = w.Essn
where w.Essn is null;

rollback;

-- e
select d.Dname, avg(e.Salary) as Avg_Salary
from EMPLOYEE e
join DEPARTMENT d on e.Dno = d.Dnumber
group by d.Dname;

-- f
select e.Fname, e.Lname, e.Address, l.Dnumber
from EMPLOYEE e
join WORKS_ON w on e.Ssn = w.Essn
join PROJECT p on w.Pno = p.Pnumber
join DEPT_LOCATIONS l on p.Dnum = l.Dnumber
join DEPARTMENT d on e.Dno = d.Dnumber
where p.Plocation = 'Houston'
      and l.Dnumber not in (
      	  select dep.Dnumber
	  from DEPARTMENT dep
	  join DEPT_LOCATIONS loc on dep.Dnumber = loc.Dnumber
	  where loc.Dlocation = 'Houston'
      );
