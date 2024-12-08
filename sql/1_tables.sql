-- Create tables without foreign keys first ---------- 

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
	total_amount number,
	location varchar(25)
);

create table grants ( -- Oracle does not allow grant as a name
	grant_ID int primary key,
	grant_number varchar(250),
	amount number
);

create table software_license (
	sw_lic_ID int primary key,
	name varchar(20),
	url varchar(100),
	gpl_compatible char(1) check (gpl_compatible in ('T', 'F'))
	-- NOTE: does Oracle not have a boolean data type?
    -- Looks like using T/F or 1/0 are the only options. Weird.
);

create table software (
	sw_ID int primary key,
	name varchar(25),
	description varchar(25)
);

create table users ( -- user is another oracle conflict. change to users
	u_ID int primary key,
	email varchar(30) -- max 26
);

create table root_dataverse (
	root_ID int primary key,
	title varchar(50),
	url varchar(50),
	description varchar(500)
);


-- Now tables with foreign keys. Work down from root ----------  

create table collection (
	col_ID int primary key,
	root_ID int references root_dataverse (root_ID), 
	title varchar(500),
	description varchar(1000),
	pub_date date,
	n_files int,
	n_datasets int,
	file_downloads int
);

create table dataset (
	ds_ID varchar(50) primary key, 
		-- ds_ID is a DOI
	col_ID int references collection (col_ID),
	lic_ID int references license (lic_ID),
	sw_lic_ID int references software_license (sw_lic_ID),
	title varchar(300), 
		-- max is 260
	description varchar(1000), 
		-- cut off descriptions at 900 chars in R
		-- maybe better off leaving it though?
	pub_date date,
	file_downloads int,
	n_files int,
	url varchar(50) -- max is 33
);

create table files (
		-- cannot use 'file' as table name, making it files
	file_ID varchar(50) primary key,
		-- this is a DOI
	ds_ID varchar(50) references dataset (ds_ID),
    filetype varchar(100) check (filetype in (
        'Adobe PDF',
        'AVI Video',
        'Comma Separated Values',
        'Fixed Field Text Data',
        'Gzip Archive',
        'JSON',
        'Markdown Text',
        'Mathematica', 
        'MATLAB Data',
        'MATLAB Source Code',
        'MS Excel Spreadsheet',
        'MS Word',
        'Network Common Data Form', 
        'Plain Text',
        'PNG Image',
        'Python Source Code', 
        'Quicktime Video', 
        'R Syntax',
        'Rich Text Format',
        'Shapefile as ZIP Archive',
        'Stata Syntax',
        'Tab-Delimited',
        'TAR Archive',
        'TIFF Image',
        'Unknown',
        'XZ Archive',
        'ZIP Archive'
    )),
	filesize numeric,
	title varchar(1000),
	description varchar(1000),
	downloads int,
	pub_date date
);

create table subjects (
	ds_ID varchar(50) references dataset (ds_ID),
	subject varchar(100) check (subject in (
        'Arts and Humanities',
        'Astronomy and Astrophysics',
        'Business and Management',
        'Chemistry',
        'Computer and Information Science',
        'Earth and Environmental Sciences',
        'Engineering',
        'Law',
        'Mathematical Sciences',
        'Medicine, Health and Life Sciences',
        'Other', 
        'Physics',
        'Social Sciences'
    )),
	primary key (ds_ID, subject)
);

create table registered_user (
	ru_ID int primary key,
	u_ID int references users (u_id),
    name varchar(50),
	privilege varchar(5) check (privilege in ('read', 'write')),
	pw_hash varchar(25)
);

create table author (
	auth_ID int primary key,
	ru_ID int references registered_user (ru_ID),
	orcid varchar(20)
		-- orcid should technically be a static 19 chars
		-- but I might have added weird numbers to these if missing
);

create table affiliation (
	int_ID int references institution (int_ID),
	ru_ID int references registered_user (ru_ID)
);

create table publication (
	pub_ID int primary key,
	ds_ID varchar(50) references dataset (ds_ID),
	citation varchar(750),
	url varchar(250)
);

create table produce (
	auth_ID int references author (auth_ID),
	pub_ID int references publication (pub_ID)
);

create table file_upload (
	auth_ID int references author (auth_ID),
	file_ID varchar(50) references files (file_ID),
	timestamp timestamp
);

create table file_download (
	file_ID varchar(50) references files (file_ID),
	u_ID int references users (u_ID),
	timestamp timestamp
);

create table dataset_upload (
	ds_ID varchar(50) references dataset (ds_ID),
	auth_ID int references author (auth_ID),
	timestamp timestamp
);

create table dataset_download (
	ds_ID varchar(50) references dataset (ds_ID),
	u_ID int references users (u_ID),
	timestamp timestamp
);

create table keywords (
	ds_ID varchar(50) references dataset (ds_ID),
	keyword varchar(100)
);

create table funds (
	grant_ID int references grants (grant_ID),
	ds_ID varchar(50) references dataset (ds_ID),
	agency_ID int,
	primary key (grant_ID, ds_ID, agency_ID)
);

create table admin (
	ru_ID int references registered_user (ru_ID),
	start_date date
);

create table manage_dataverse (
	ru_ID int references registered_user (ru_ID),
	root_ID int references root_dataverse (root_ID),
	timestamp timestamp,
	description varchar(1000)
);

create table manage_collection (
	col_ID int references collection (col_ID),
	ru_ID int references registered_user (ru_ID),
	timestamp timestamp,
	description varchar(500)
);


create table contact (
	u_ID int references users (u_ID),
	ds_ID varchar(50) references dataset (ds_ID),
	timestamp timestamp,
	message varchar(500)
);

create table analyzes (
	sw_ID int references software (sw_ID),
	ds_ID varchar(50) references dataset (ds_ID),
	title varchar(500),
	description varchar(1000),
	repo_url varchar(250)
);
		


