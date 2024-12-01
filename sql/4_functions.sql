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


-- 5. datasetActivity ----------------------------------

-- Note that the download logs are fabricated
-- Also, all downloads occur after Nov 1st, 2024. Example dataset.ds_id below:
-- 'doi:10.7281/T1/6BHBC4', 'doi:10.7281/T10Z715B', 'doi:10.7281/T1/VWQ4J3'

create or replace function datasetActivity(
	id in varchar default null,
	start_date in date default null,
	end_date in date default null
) return number
as
	start_timestamp timestamp;
	end_timestamp timestamp;
	diff_days number;
	downloads_in_window int;
	activity number(6, 3);
begin

	-- Make sure id is provided
	if id is null then
		raise_application_error(-20001, 'Please provide a dataset ID.');
	end if;

	-- If no start date, use pub_date from dataset, else just convert to timestamp
	if start_date is null then
		select cast(pub_date as timestamp)
		into start_timestamp
		from dataset
		where ds_ID = id;
	else 
		start_timestamp := cast(start_date as timestamp);
	end if;

	-- If no end date, use current timestamp
	if end_date is null then
		select current_timestamp
		into end_timestamp
		from dual;
	else 
		end_timestamp := cast(end_date as timestamp);
	end if;

	-- Calculate how many days are between start and end 
	diff_days := extract(day from (end_timestamp - start_timestamp));

	-- Get downloads in that time
	select count(*)
	into downloads_in_window
	from dataset_download
	where ds_ID = id
		and timestamp > start_timestamp
		and timestamp <= end_timestamp;
	
	-- Calculate downloads per day in that window
	activity := downloads_in_window / diff_days;
	return activity;

end;
/


-- 6. proportionGPL -----------------------------------

create or replace function proportionGPL(
	subject_in in varchar default null
) return number
as
	dummy int := 0;
	prop_gpl number(4, 2);
begin
	
	-- If no input, get prop for whole database
    if subject_in is null then
		dbms_output.put_line('SUBJECT IN IS NULL');
		select sum(case when sw.name = 'GPL-3.0' then 1 else 0 end) / count(*)
		into prop_gpl
		from software_license sw, dataset d
		where sw.sw_lic_ID = d.sw_lic_ID;
		return prop_gpl;

	-- Prop by subject 
	elsif subject_in is not null then

		-- Check whether subject is in subjects table
		begin
    		select count(*) 
    		into DUMMY
			from subjects
    		where subject = subject_in;
		exception 
			when NO_DATA_FOUND then
				DUMMY := 0;
		end;

		-- If DUMMY is 1 (subject exists), run function and return
		if DUMMY >= 1 then
			select sum(case when sw.name = 'GPL-3.0' then 1 else 0 end) / count(*)
			into prop_gpl
			from software_license sw, dataset d, subjects s
			where s.ds_ID = d.ds_ID
				and sw.sw_lic_ID = d.sw_lic_ID
				and subject_in = s.subject;
			return prop_gpl;

		-- If DUMMY is 0, (no subject), throw error
		elsif DUMMY = 0 then	
			raise_application_error(
				-20001, 
				'Subject not found in Dataverse.'
			);
		end if;

	end if;
end;
/


-- 7. fundingImpact --------------------------

create or replace function fundingImpact(
    grant_id_in in int default null,
    agency_name_in in varchar default null,
    metric in varchar default null
) return varchar
as
    row_check int;
    conditions varchar(1000);
    query varchar(1000);
    result int;
begin
    
    -- Make sure is either a grant id or name input
    if grant_id_in is null and agency_name_in is null then
		raise_application_error(
			-20001, 
			'Please provide either a grant ID or a funging agency name.'
		);
    end if;

    -- Metric input can either be publications or downloads
    if metric not in ('publications', 'downloads') then
        raise_application_error(
            -20002,
            'Please provide a metric as either "publications" or "downloads".'
        );
    end if;

    -- If both agency id and grant number are used, make sure they coincide
    if grant_id_in is not null and agency_name_in is not null then
        select count(*)   
        into row_check
        from dataset d, funds f, funding_agency fa
        where d.ds_id = f.ds_ID
            and f.agency_ID = fa.agency_ID
            and f.grant_ID = grant_id_in
            and fa.name = agency_name_in;
        if row_check = 0 then
            raise_application_error(-20003, 'No records found.');
        end if;
    end if;

    -- Conditions based on inputs to use in query
    conditions := '';

    if grant_id_in is not null then
        conditions := conditions || ' and f.grant_ID = ''' || grant_id_in || '''';
    end if;

    if agency_name_in is not null then
        conditions := conditions || ' and fa.name = ''' || agency_name_in || '''';
    end if;

    -- If metric is publications, create query
    if metric = 'publications' then
        
        query := 
            'select count(*) 
            from dataset d, publication p, funds f, funding_agency fa
            where d.ds_ID = p.ds_ID
                and d.ds_ID = f.ds_ID
                and fa.agency_ID = f.agency_ID 
                ' || conditions;

    -- If metric is downloads, create query
    elsif metric = 'downloads' then

        query := 
            'select sum(d.file_downloads)
            from dataset d, funds f, funding_agency fa
            where d.ds_ID = f.ds_ID
                and f.agency_ID = fa.agency_ID
                ' || conditions;

    end if;

    dbms_output.put_line('Query: ' || query);

    -- Execute query
    execute immediate query into result;

    -- Test outputs
    return result;

end;
/


-- 8. totalSize -------------------------------------------------

create or replace function totalSize(
	scope in varchar default null,
	id in varchar default null
) return number
as
	TOTALSIZE number;
	IDCHECK int := 0;
begin
	-- Checking bad inputs --------
	-- if user gives id but not scope, throw error
	if scope is null and id is not null then
		raise_application_error(
			-20001,
			'If scope is specified, id must be provided.'
		);

	-- if scope but no id, also error
	elsif scope is not null and id is null then
		raise_application_error(
			-20001,
			'If id is specified, scope must be provided.'
		);
	
	-- if no inputs, get filesize of whole dataverse
	elsif scope is null and id is null then
		select sum(filesize)
		into TOTALSIZE
		from files;
		return TOTALSIZE;

	-- Check that scope input is one of three available options
	elsif scope not in ('dataset', 'collection', 'dataverse') then
		raise_application_error(
			-20001, 
			'Input for scope must be dataset, collection, or dataverse'
		);
	end if;


	-- Start calculations based on scope now --------
	-- dataset
	if scope = 'dataset' then 
		
		-- Save a 1/0 value for whether the id exists
		begin
			select 1
			into IDCHECK 
			from dataset
			where ds_ID = id;
		
		-- include exception to set IDCHECK to 0 if nothing found
		exception
			when NO_DATA_FOUND then
				IDCHECK := 0;
		end;

		-- if id exists, calculate total size	
		if IDCHECK = 1 then
			select sum(filesize)
			into TOTALSIZE
			from files
			where ds_ID = id;

		-- if id does not exist, throw error
		elsif IDCHECK = 0 then
			raise_application_error(
				-20001,
				'ds_ID is not found in dataset table.'
			);
		
		end if;
	end if; 	
	
	-- collection
	if scope = 'collection' then
		begin
			select 1
			into IDCHECK 
			from collection
			where col_ID = id;
		exception
			when NO_DATA_FOUND then
				IDCHECK := 0;
		end;

		if IDCHECK = 1 then
			select sum(f.filesize)
			into TOTALSIZE
			from files f, dataset d, collection c
			where c.col_ID = d.col_ID
				and d.ds_ID = f.ds_ID
				and c.col_ID = id;
		
		elsif IDCHECK = 0 then
			raise_application_error(
				-20001,
				'col_ID is not found in collection table.'
			);
		
		end if; 
	end if; 

	-- dataverse
	if scope = 'dataverse' then
		begin
			select 1
			into IDCHECK 
			from root_dataverse
			where root_ID = id;
		exception
			when NO_DATA_FOUND then
				IDCHECK := 0;
		end;

		if IDCHECK = 1 then
			select sum(f.filesize)
			into TOTALSIZE
			from files f, dataset d, collection c, root_dataverse r
			where f.ds_ID = d.ds_ID
				and d.col_ID = c.col_ID
				and c.root_ID = r.root_ID
				and r.root_ID = id;
		
		elsif IDCHECK = 0 then
			raise_application_error(
				-20001,
				'root_ID is not found in root_dataverse table.'
			);
		
		end if; 
	end if; 

	return TOTALSIZE;
end;
/


