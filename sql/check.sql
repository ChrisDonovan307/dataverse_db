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

