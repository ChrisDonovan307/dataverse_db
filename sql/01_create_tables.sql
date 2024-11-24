-- Create Tables  -------------------------------------------------- 

-- First are the easy tables without any foreign keys
create table institution (
	int_ID int primary key,	
	name varchar(1000),
	address varchar(500)
);

create table license (
	lic_ID int primary key,
	name varchar(15),
	url varchar(250)
);

create table funding_agency (
	agency_ID int primary key,
	name varchar(250),
	total_amount int,
	location varchar(25)
);

create table grants ( 		-- Oracle does not allow grant as a name
	grant_ID int primary key,
	grant_number varchar(100),
	amount int
);

create table software_license (
	sw_lic_ID int primary key,
	name varchar(20),
	url varchar(100),
	gpl_compatible varchar(5) check (gpl_compatible in ('TRUE', 'FALSE'))
	-- Note apparently Oracle does not have boolean column type?
	-- I will go back and change this column to T/F so we can make it char(1)
	-- for now just seeing if this will work as is
);

create table software (
	sw_ID int primary key,
	name varchar(25),
	description varchar(25)
);

create table users (		-- user is another oracle conflict
	u_ID int primary key,
	email varchar(25)
);

create table root_dataverse (
	root_ID int primary key,
	title varchar(50),
	url varchar(50),
	description varchar(500)
);


-- Now tables with foreign keys. Work down from root

create table collection (
	col_ID int primary key,
	title varchar(500),
	pub_date date,
	description varchar(1000),
	-- root_ID varchar(25) references root_dataverse (root_ID), -- have to fix this column in collection
	n_files int,
	downloads int
);


create table dataset (
	ds_ID varchar(50) primary key,
	title varchar(500), 
	url varchar(250),
	description varchar(1000),
	pub_date date,
	downloads int,
	n_files int,
	col_ID int references collection (col_ID),
	lic_ID int references license (lic_ID),
	sw_lic_ID int references software_license (sw_lic_ID)
);

create table files ( 	-- cannot use 'file' as table name, making it files
	file_ID varchar(50) primary key,
	ds_ID varchar(50) references dataset (ds_ID),
       	filetype varchar(50),
	filesize numeric,
	title varchar(1000),
	description varchar(1000),
	downloads int,
	pub_date date
);

create table subject (
	ds_ID varchar(50),
	subject varchar(50),
	primary key (ds_ID, subject)
);

-- Stopping here for now to see if the insert statements work


