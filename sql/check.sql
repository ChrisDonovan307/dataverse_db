-- Check that package exists -----------------------------------
select object_name, status 
from user_objects 
where object_type = 'PACKAGE';


-- Check indexes -----------------------------------------------

select index_name from user_indexes;


-- f1. ---------------------------------------------------------

-- Each pair of 2 should give the same result
select dataverse.fundingDistribution(null, 2, null, 60) from dual;
select dataverse.fundingDistribution(
    'American Chemical Society', 
    null, 
    'Sedimentary Geobiology Group', 
    null
) from dual;

select dataverse.fundingDistribution(null, 9, null, 5) from dual;
select dataverse.fundingDistribution(
    'Department of Energy',
    null,
    'Dr. Howard Katz DOE-supported projects',
    null
) from dual;

-- Check what happens when two agency values are provided, or two collection values
-- Should still work
select dataverse.fundingDistribution(
    'American Chemical Society', 
    2, 
    null, 
    60
) from dual;

select dataverse.fundingDistribution(
    null, 
    2, 
    'Sedimentary Geobiology Group', 
    60
) from dual;

-- Should throw error if there are no records
select dataverse.fundingDistribution(null, 9, null, 60) from dual;

-- f2. -------------------------------------------------------------
 INSERT INTO users(u_ID, email) VALUES(7777, 'no@bo.dy');
 INSERT INTO registered_user(ru_ID, u_ID, name, pw_hash) VALUES (8888, 7777, 'Norman nobody', 'PASSWORD');
 SELECT dataverse.userLoginAttempt(8888, 'PASSWORD') FROM dual; -- should return 1
 SELECT dataverse.userLoginAttempt(8888, 'p@ssword') FROM dual; -- should return 0 -- password do not match
 SELECT dataverse.userLoginAttempt(6666, 'password') FROM dual; -- should return 0 -- no such user exists
 
-- f3. -------------------------------------------------------------
 SELECT dataverse.generateNewPassword(579) FROM dual; -- expect a random alphanumeric string
 
-- f4. -------------------------------------------------------------
-- random keywords and author name from somewhere within this dataverse
-- expect to receive a list of locations with counted REFERENCES
-- for each item
-- Encounters an error after merging routines into a package
-- SELECT dataverse.search('brain, BLAM, Katz') FROM dual; 

-- f5. --------------------------------------------------------

-- Note that all downloads in log are after Nov 1st 2024.
-- First one should throw error for no input
select dataverse.datasetActivity() from dual;

-- Next four should work
select dataverse.datasetActivity('doi:10.7281/T10Z715B') from dual;
select dataverse.datasetActivity('doi:10.7281/T1/6BHBC4') from dual;
select dataverse.datasetActivity( 'doi:10.7281/T1/6BHBC4', '01-NOV-24', '15-NOV-24') from dual;
select dataverse.datasetActivity( 'doi:10.7281/T1/6BHBC4', '01-NOV-24') from dual;

-- Last one should return 0 - no downloads before 01-NOV-24
select dataverse.datasetActivity( 'doi:10.7281/T1/6BHBC4', null, '01-NOV-24') from dual;


-- f6. -----------------------------------------------------------

-- First two should be 0.6
select dataverse.proportionGPL(null) from dual;
select dataverse.proportionGPL() from dual;

-- Chemistry should be 0
select dataverse.proportionGPL('Chemistry') from dual; 

-- This should throw error, not real subject
select dataverse.proportionGPL('zzzzzzz') from dual;


-- f7. -------------------------------------------------------------

-- These should work
select dataverse.fundingImpact(1, null, 'publications') from dual;
select dataverse.fundingImpact(1, null, 'downloads') from dual;

-- Explore NSF with different combinations of inputs. Should work
select dataverse.fundingImpact(25, null, 'downloads') from dual;
select dataverse.fundingImpact(null, 'National Science Foundation', 'downloads') from dual;
select dataverse.fundingImpact(25, 'National Science Foundation', 'downloads') from dual;

-- Try NSF with a wrong grant ID number. Should throw error.
select dataverse.fundingImpact(1, 'National Science Foundation', 'downloads') from dual;


-- f8. -------------------------------------------------------------

-- Null null should give size of whole dataverse 
select dataverse.totalSize(null, null) from dual;

-- Get size of first collection
select dataverse.totalSize('collection', 1) from dual;

-- This should not work - there is no dataset id 1
select dataverse.totalSize('dataset', 1) from dual;

-- This should not work - not in allowed scope inputs
select dataverse.totalSize('zzzzzz', 1) from dual;


-- p1. --------------------------------------------------------------

-- p2. --------------------------------------------------------------

-- p3. --------------------------------------------------------------
 -- dumpEmptyFiles
 DELETE FROM empty_files; -- purge any previous records
 EXEC dataverse.dumpEmptyFiles;
 SELECT COUNT(*) FROM empty_files; -- 12 seems to be the correct NUMBER
 SELECT COUNT(*) FROM files WHERE filesize=0; -- agrees with 12
 
-- p4. --------------------------------------------------------------
 -- keywordSummary
 SELECT * FROM keywords WHERE ds_ID='doi:10.7281/T1/0EYOMQ'; -- irrigation, Noah-MP, data assimilation, vegetation, soil moisture
 EXEC dataverse.keywordSummary('doi:10.7281/T1/0EYOMQ');
 SELECT COUNT(*) FROM keyword_count; -- returns 5 if only the previous dataset has been summarized
 SELECT count_of FROM keyword_count WHERE keyword='irrigation'; -- returns 3

-- p5. --------------------------------------------------------------
 -- cleanKeywords
SELECT * FROM keywords FETCH FIRST 3 ROWS ONLY; -- View the previous state
DECLARE
	temp strings_t := strings_t();
BEGIN
-- values are hardcoded in from the first 3 rows and may need adjustment
	temp.extend;
	temp(1) := 'electrochemical stability';
	temp.extend;
	temp(2) := 'physical polymer chemistry';
	temp.extend;
	temp(3) := 'polycarboxylic acids';
	
	cleanKeywords('SUCCESSFUL OVERWRITE', temp);
END;
/
 SELECT * FROM keywords FETCH FIRST 3 ROWS ONLY; -- Note that two of the three previous rows are deleted to prevent duplicate primary keys

-- p6. --------------------------------------------------------------

-- Bad input errors
exec dataverse.softwareUse('badbad', null);
exec dataverse.softwareUse(null, 'badbad');

-- Should work
exec dataverse.softwareUse(null, null);
exec dataverse.softwareUse(null, 'Social Sciences');
exec dataverse.softwareUse(null, 'Physics');
exec dataverse.softwareUse('COVID-19', null);

-- Subject and keyword that don't match, should not work
exec dataverse.softwareUse('COVID-19', 'Physics');


-- p7. -------------------------------------------------------------- 

-- First three should work, last one should not (no covid/physics datasets)
exec dataverse.softwareLicenses(null, null);
exec dataverse.softwareLicenses('COVID-19', null);
exec dataverse.softwareLicenses(null, 'Social Sciences');
exec dataverse.softwareLicenses('COVID-19', 'Physics');


-- p8. --------------------------------------------------------------

-- First four should work, not the last one
exec dataverse.funderSummary(null, null);
exec dataverse.funderSummary('National Science Foundation', null);
exec dataverse.funderSummary(null, 'Physics');
exec dataverse.funderSummary('National Eye Institute', null);
exec dataverse.funderSummary('National Eye Institute', 'Physics');


-- p9. --------------------------------------------------------------

-- First two should show results, not third
begin
    dataverse.searchAuthors('david');
end;
/

begin
    dataverse.searchAuthors('emily');
end;
/

begin
    dataverse.searchAuthors('zzzzzzzz');
end;
/

-- t1. ---------------------------------------------------------------
 -- hashing test
 -- create a fake user with corresponding entry in registered user
 -- select pw_hash for that user to ensure that hashing has occured
 INSERT INTO users(u_ID, email) VALUES(99999, 'ann@nom.com');
 INSERT INTO registered_user(ru_ID, u_ID, name, privilege, pw_hash) VALUES (99999, 99999, 'Ann Nominous', 'write', '123fakestreet');
 SELECT * FROM registered_user WHERE ru_ID = 99999;

-- t6-9. -------------------------------------------------------------
-- timestamp test
-- create a fake download entry. file_ID and u_ID must be valid entries in files and users respectively
 INSERT INTO file_download(file_ID, u_ID) VALUES('doi:10.7281/T1/L4QB1Z/1LIJI8', 580);
 -- select all columns of files downloaded by user 580
 SELECT * FROM file_download WHERE u_ID = 580;


-- t2. -------------------------------------------------------------

-- check number of datasets in a certain collection
select n_datasets
from collection
where col_ID = 1;

-- Rollback point to return to
savepoint t2;

-- insert into files
insert into dataset values (
	'test',
	(select col_ID from dataset where col_ID = 1 fetch first 1 row only),
	null, 
	null, 
	null, 
	null, 
	null,
    null,	
	null,
	null
);


-- See if it worked
select n_datasets
from collection
where col_ID = 1;

-- delete from files
delete from dataset
where ds_ID = 'test';

-- See if it worked
select n_datasets
from collection
where col_ID = 1;

-- Rollback to before changes
rollback to savepoint t2;


-- t3. --------------------------------------------------------------

-- check number of files in a certain dataset
select n_files
from dataset
where ds_ID = 'doi:10.7281/T1/6Y08LS';

-- save point
savepoint t3;

-- insert into files
insert into files values ('test', 'doi:10.7281/T1/6Y08LS', null, null, null, null, null, null);

-- See if it worked
select n_files
from dataset
where ds_ID = 'doi:10.7281/T1/6Y08LS';

-- delete from files
delete from files
where file_ID = 'test';

-- See if it worked
select n_files
from dataset
where ds_ID = 'doi:10.7281/T1/6Y08LS';

-- Rollback
rollback to savepoint t3;


-- t4. --------------------------------------------------------------

-- check number of files in a certain collection
select n_files
from collection
where col_ID = 1;

-- savepoint
savepoint t4;

-- insert into files
insert into files values (
	'test',
	(select ds_ID from dataset where col_ID = 1 fetch first 1 row only),
	null, 
	null, 
	null, 
	null, 
	null, 
	null
);


-- See if it worked
select n_files
from collection
where col_ID = 1;

-- delete from files
delete from files
where file_ID = 'test';

-- See if it worked
select n_files
from collection
where col_ID = 1;

-- Rollback
rollback to savepoint t4;


-- t5. ---------------------------------------------------------------

-- check total_amount from a certain agency_id in funding_agency table
select total_amount
from funding_agency
where agency_ID = 4;

-- savepoint
savepoint t5;

-- add a record to grant table, then another to funds table
insert into grants values (
	999,
	'test',
	42
);

insert into funds values (
	999,
	'doi:10.7281/T10Z715B',
	4
);

-- check total_amount for agency_ID 1 again
select total_amount
from funding_agency
where agency_ID = 4;

-- Rollback
rollback to savepoint t5;
