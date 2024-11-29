select e.Ssn
from EMPLOYEE e
join (
     select Dno, avg(Salary) as avg_salary
     from EMPLOYEE
     group by Dno
     ) salaries on e.Dno = salaries.Dno
where e.Salary > salaries.avg_salary
and (select count(*) from DEPENDENT d where d.Essn = e.Ssn) >= 2;

select e.Ssn
from EMPLOYEE e, WORKS_ON w, PROJECT p
where e.Ssn = w.Essn
      and w.Pno = p.Pnumber
      and p.Dnum != e.Dno
group by e.Ssn
having sum(w.Hours) > 10;
