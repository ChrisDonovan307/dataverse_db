-- 1. fundingDistribution --------------------------------------

-- Note: all grant amounts are fabricated
-- Need both agency info and collection info to run. Examples:
-- 1. 'American Chemical Society', '2', 'Sedimentary Geobiology Group', '60'
-- 2. 'Department of Energy', '9', 'Dr. Howard Katz DOE-supported projects', '5'

create or replace function fundingDistribution(
	agency_in in varchar default null,
	agency_id_in in int default null,
	col_in in varchar default null,
	col_id_in in int default null
) return number
as
	agency_condition varchar(100);
	col_condition varchar(100);
	query varchar(2000);
	n_rows int;
	result number(6, 2);
begin
		
	-- Make sure there is at least one agency arg and at least one col arg
	if agency_in is null and agency_id_in is null then
		raise_application_error(
			-20001, 
			'Please provide either an agency name or agency id.'
		);
	elsif col_in is null and col_id_in is null then
		raise_application_error(
			-20002,
			'Please provide either a collection name or collection id.'
		);
	end if;

	-- Agency condition for dynamic query
	if agency_in is not null then
		agency_condition := 'fa.name = ''' || agency_in || '''';
	else
		agency_condition := 'fa.agency_ID = ''' || agency_id_in || '''';
	end if;

	-- Collection condition for dynamic query
	if col_in is not null then
		col_condition := 'c.title = ''' || col_in || '''';
	else
		col_condition := 'c.col_ID = ''' || col_id_in || '''';
	end if;

	-- Dynamic query for rows. If 0, return an error. (agency does not fund in that col)
    query := 
		'select count(*) 
		from funding_agency fa, funds f, dataset d, collection c
           where ' || agency_condition || ' 
           	and ' || col_condition || ' 
           	and fa.agency_ID = f.agency_ID
			and f.ds_ID = d.ds_ID
           	and d.col_ID = c.col_ID';
    execute immediate query into n_rows;

    -- If no rows returned, throw error
    if n_rows = 0 then
        raise_application_error(
				-20003, 
				'No matching records found. No agency funding within that collection.'
		);
    end if;

	-- Dynamic query to get prop of datasets in collection sponsored by agency
	query := 
		'select sum(case when ' || agency_condition || ' then 1 else 0 end) / count(*)
		from funding_agency fa, funds f, dataset d, collection c
		where ' || col_condition || '
			and fa.agency_ID = f.agency_ID
			and f.ds_ID = d.ds_ID
			and d.col_ID = c.col_ID';

	-- Run query and return result
	execute immediate query into result; 
	return result;

end;
/

show errors

-- Test. Each pair of 2 should give the same result
select fundingDistribution(null, 2, null, 60) from dual;
select fundingDistribution(
		'American Chemical Society', 
		null, 
		'Sedimentary Geobiology Group', 
		null) from dual;

select fundingDistribution(null, 9, null, 5) from dual;
select fundingDistribution(
		'Department of Energy',
		null,
		'Dr. Howard Katz DOE-supported projects',
		null) from dual;

-- Check what happens when two agency values are provided, or two collection values
select fundingDistribution('American Chemical Society', 2, null, 60) from dual;
select fundingDistribution(null, 2, 'Sedimentary Geobiology Group', 60) from dual;

-- Check when agency has no funded datasets in that collection
select fundingDistribution(null, 9, null, 60) from dual;

-- Reset
drop function fundingDistribution;

