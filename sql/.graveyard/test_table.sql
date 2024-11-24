-- Create file table

create table files (
	file_ID varchar(50) primary key,
	ds_ID varchar(50),
       	filetype varchar(50),
	filesize numeric,
	title varchar(250),
	description varchar(1000),
	downloads int,
	pub_date timestamp
);
INSERT INTO files VALUES ('doi:10.7281/T10Z715B/HHGUOR', 'doi:10.7281/T10Z715B', 'ZIP Archive', ' 4603476', 'IntraUrbanTemperatureVariabilityBaltimore.zip', 'Data, metadata, Readme and sample code used in publication Scott et al 2016.', '30', '20-Jul-2016 12:00:00');

INSERT INTO files VALUES ('doi:10.7281/T15Q4T12/0BMVQF', 'doi:10.7281/T15Q4T12', 'ZIP Archive', ' 7582841', 'data_package_from_ankarali_2015.zip', 'This zip file contains MATLAB scripts (.m), data inputs (.mat), and data outputs (.mat) associated with the statistical methods and plots from the publication. The input data is also available in .csv format. See the read_me file within this .zip for detailed information on this data package and its contents.', '38', '18-Jun-2015 12:00:00');

INSERT INTO files VALUES ('doi:10.7281/T1J10120/3UUMZV', 'doi:10.7281/T1J10120', 'ZIP Archive', '22659543', 'morgan-pally-bpd-data-distribution-2016-v1.zip', 'Zip file containing: (1) the report and its appendices, (2) raw data files from the Baltimore Police Department downloaded from the Open Baltimore portal on February 3, 2016, and (3) an R project with associated analysis code that reproduces the key results of the report.', '92', '15-Mar-2016 12:00:00');

select file_ID, downloads, pub_date
from files;

select filesize, downloads, pub_date
from files;

drop table files;
	
