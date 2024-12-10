-- Create clustered index ----------------------------

-- Clustering on any tables with a ds_id field
-- Note that more indexes are included in 3_indexes.sql

create cluster ds_cluster (ds_ID varchar(50));
create index ds_cluster_idx
on cluster ds_cluster;


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

create table grants (
	grant_ID int primary key,
	grant_number varchar(250),
	amount number
);

create table software_license (
	sw_lic_ID int primary key,
	name varchar(20),
	url varchar(100),
	gpl_compatible char(1) check (gpl_compatible in ('T', 'F'))
);

create table software (
	sw_ID int primary key,
	name varchar(25),
	description varchar(25)
);

create table users ( 
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
	description varchar(1000), 
	pub_date date,
	file_downloads int,
	n_files int,
	url varchar(50)
)
cluster ds_cluster(ds_ID);

create table files (
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
)
cluster ds_cluster(ds_ID);

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
)
cluster ds_cluster(ds_ID);

create table registered_user (
	ru_ID int primary key,
	u_ID int references users (u_id),
    name varchar(50),
	pw_hash varchar2(64)
);

create table author (
	auth_ID int primary key,
	ru_ID int references registered_user (ru_ID),
	orcid varchar(20)
);

create table affiliation (
	int_ID int references institution (int_ID),
	ru_ID int references registered_user (ru_ID),
    primary key (int_ID, ru_ID)
);

create table publication (
	pub_ID int primary key,
	ds_ID varchar(50) references dataset (ds_ID),
	citation varchar(750),
	url varchar(250)
)
cluster ds_cluster(ds_ID);

create table produce (
	auth_ID int references author (auth_ID),
	pub_ID int,
	primary key (auth_ID, pub_ID),
	constraint RFK_produce foreign key (pub_ID) references publication (pub_ID)
);

create table file_upload (
	auth_ID int references author (auth_ID),
	file_ID varchar(50) references files (file_ID),
	timestamp timestamp,
    primary key (auth_ID, file_ID, timestamp)
);

create table file_download (
	file_ID varchar(50) references files (file_ID),
	u_ID int references users (u_ID),
	timestamp timestamp,
	primary key (file_ID, u_ID, timestamp)
);

create table dataset_upload (
	ds_ID varchar(50) references dataset (ds_ID),
	auth_ID int references author (auth_ID),
	timestamp timestamp,
    primary key (ds_ID, auth_ID, timestamp)
)
cluster ds_cluster(ds_ID);

create table dataset_download (
	ds_ID varchar(50) references dataset (ds_ID),
	u_ID int references users (u_ID),
	timestamp timestamp,
    primary key (ds_ID, u_ID, timestamp)
)
cluster ds_cluster(ds_ID);

create table keywords (
	ds_ID varchar(50) references dataset (ds_ID),
	keyword varchar(100),
    primary key (ds_ID, keyword)
)
cluster ds_cluster(ds_ID);

create table funds (
	grant_ID int references grants (grant_ID),
	ds_ID varchar(50) references dataset (ds_ID),
	agency_ID int references funding_agency(agency_ID),
	primary key (grant_ID, ds_ID, agency_ID)
)
cluster ds_cluster(ds_ID);

create table admin (
	ru_ID int references registered_user (ru_ID),
	start_date date,
    primary key (ru_ID, start_date)
);

create table manage_dataverse (
	ru_ID int references registered_user (ru_ID),
	root_ID int references root_dataverse (root_ID),
	timestamp timestamp,
	description varchar(1000),
    primary key (ru_ID, timestamp)
);

create table manage_collection (
	col_ID int references collection (col_ID),
	ru_ID int references registered_user (ru_ID),
	timestamp timestamp,
	description varchar(500),
	primary key (ru_ID, col_ID, timestamp)
);

create table contact (
	u_ID int references users (u_ID),
	ds_ID varchar(50) references dataset (ds_ID),
	timestamp timestamp,
	message varchar(500),
    primary key (u_ID, ds_ID, timestamp)
)
cluster ds_cluster(ds_ID);

create table analyzes (
	sw_ID int references software (sw_ID),
	ds_ID varchar(50) references dataset (ds_ID),
	title varchar(500),
	description varchar(1000),
	repo_url varchar(250),
    primary key (sw_ID, ds_ID)
)
cluster ds_cluster(ds_ID);

create table keyword_count (
	keyword VARCHAR(100), 
	count_of number
);

create table empty_files AS SELECT * FROM files;
		


