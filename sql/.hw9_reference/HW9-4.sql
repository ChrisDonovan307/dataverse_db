-- 4. Stored Function --------------------------------------------------

-- a
create or replace function deptavgsal(
  depnum in DEPARTMENT.dNumber%type default null
) return number
as
  avgsal number;
  dep_match number;
begin
  if depnum is null then
    return -1;
  else
    select count(*) into dep_match
    from DEPARTMENT
    where dNumber = depnum;

    if dep_match = 0 then
       return -2;

    else
      select avg(e.Salary) into avgsal
      from EMPLOYEE e, DEPARTMENT d
      where e.Dno = d.Dnumber
      and d.Dnumber = depnum;
      
      return avgsal;
    end if;  
  end if;
end;
/

show errors


-- b
select deptavgsal() from dual;
select deptavgsal(2) from dual;
select deptavgsal(3) from dual;
select deptavgsal(6) from dual;
select deptavgsal(1) from dual;
select deptavgsal(4) from dual;
select deptavgsal(5) from dual;


-- c
select Dname, deptavgsal(Dnumber)
from DEPARTMENT
where deptavgsal(Dnumber) > 32000
order by Dnumber asc;


-- d
drop function deptavgsal;

