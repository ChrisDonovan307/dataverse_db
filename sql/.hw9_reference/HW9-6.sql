-- 6. Cleanup --------------------------------------------------

drop table DEPENDENT;
drop table DEPT_LOCATIONS;
drop table WORKS_ON;
drop table PROJECT;

-- Remove FK from EMPLOYEE so we can drop DEPARTMENT
alter table EMPLOYEE
drop constraint EMP_FK2;

drop table DEPARTMENT;
drop table EMPLOYEE;


