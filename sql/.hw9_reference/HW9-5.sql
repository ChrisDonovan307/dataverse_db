-- 5. Triggers --------------------------------------------------

-- a
create table audit_trail (audit_record varchar(500));


-- b
create or replace trigger before_table
before update of Salary on EMPLOYEE
begin
  insert into audit_trail (audit_record)
  values ('Before_table trigger fired.');
end;
/

create or replace trigger after_table
after update of Salary on EMPLOYEE
begin
  insert into audit_trail (audit_record)
  values ('After_table trigger fired.');
end;
/

create or replace trigger before_row
before update of Salary on EMPLOYEE
for each row
begin
  insert into audit_trail (audit_record)
  values ('Before_row trigger fired.');
end;
/

create or replace trigger after_row
after update of Salary on EMPLOYEE
for each row
begin
  insert into audit_trail (audit_record)
  values ('After_row trigger fired.');
end;
/


-- c
update EMPLOYEE
set Salary = Salary * 1.1;


-- d
select * from audit_trail;


-- e
drop table audit_trail;
drop trigger before_table;
drop trigger after_table;
drop trigger before_row;
drop trigger after_row;


-- f
update EMPLOYEE
set Salary = Salary * 10/11;
