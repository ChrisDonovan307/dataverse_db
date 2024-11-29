-- Drop tables --------------------------------------------------

drop table DEPENDENT cascade constraints;
drop table DEPT_LOCATIONS cascade constraints;
drop table WORKS_ON cascade constraints;
drop table PROJECT cascade constraints;

-- Remove FK from EMPLOYEE so we can drop DEPARTMENT
--alter table EMPLOYEE
--drop constraint EMP_FK2;

drop table DEPARTMENT cascade constraints;
drop table EMPLOYEE cascade constraints;


