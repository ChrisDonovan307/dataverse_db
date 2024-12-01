-- Create Tables --------------------------------------------------

create table EMPLOYEE (
       Fname varchar2(50),
       Minit char(1),
       Lname varchar2(50),
       Ssn char(9) primary key,
       Bdate date,
       Address varchar2(100),
       Sex char(1) check (Sex in ('M', 'F')),
       Salary decimal(10,2) check (Salary > 20000),
       Super_ssn char(9),
       Dno int,
       -- Name the FK so we can remove it later to insert tuples
       constraint EMP_FK1 foreign key (Super_ssn) references EMPLOYEE (Ssn)
);

create table DEPENDENT (
       Essn char(9),
       Dependent_name varchar2(25),
       Sex char(1) check (Sex in ('M', 'F')),
       Bdate date,
       Relationship varchar2(8) check (Relationship in ('Spouse', 'Son', 'Daughter')),
       primary key (Essn, Dependent_name)
);

create table DEPARTMENT (
       Dname varchar2(25),
       Dnumber int primary key,
       Mgr_ssn char(9) references EMPLOYEE (Ssn),
       Mgr_start_date date
);

-- Once DEPARTMENT is made, we can add FK to EMPLOYEE
alter table EMPLOYEE
add constraint EMP_FK2
foreign key (Dno)
references DEPARTMENT (Dnumber);

create table DEPT_LOCATIONS (
       Dnumber int references DEPARTMENT (Dnumber),
       Dlocation varchar2(10) check (Dlocation in ('Bellaire', 'Sugarland', 'Houston', 'Stafford')),
       primary key (Dnumber, Dlocation)
);       

create table PROJECT (
       Pname varchar2(25),
       Pnumber int primary key,
       Plocation varchar2(10) check (Plocation in ('Bellaire', 'Sugarland', 'Houston', 'Stafford')),
       Dnum int references DEPARTMENT (Dnumber)
);

create table WORKS_ON (
       Essn char(9) references EMPLOYEE (Ssn),
       Pno int references PROJECT (Pnumber),
       Hours decimal(3,1) check (Hours >= 5 and Hours <= 40),
       primary key (Essn, Pno)
);

