-- Package --------------------------------------------------------

create or replace package dataverse as

    -- Procedures
    procedure softwareUse(
        keyword_in varchar default null,
        subject_in varchar default null
    );
    procedure softwareLicenses(
        keyword_in varchar default null, 
        subject_in varchar default null
    );
    procedure funderSummary(
        agency_name_in varchar default null, 
        subject_in varchar default null
    );
    procedure searchAuthors(
        search_term varchar
    );

    -- Functions
    function fundingDistribution(
        agency_in in varchar default null,
        agency_id_in in int default null,
        col_in in varchar default null,
        col_id_in in int default null
    ) return number; 

    function datasetActivity(
        id in varchar default null,
        start_date in date default null,
        end_date in date default null
    ) return number;

    function proportionGPL(
        subject_in varchar default null
    ) return number;

    function fundingImpact(
        grant_id_in in int default null,
        agency_name_in in varchar default null,
        metric in varchar default null
    ) return varchar;

    function totalSize(
        scope in varchar default null,
        id in varchar default null
    ) return number;

end dataverse;
/


-- Body -----------------------------------------------------------

create or replace package body dataverse as
    
    -- Procedures --------------------------------------------------
    -- p6. softwareUse ---------------------------------------------

    procedure softwareUse (
        keyword_in in varchar default null,
        subject_in in varchar default null
    ) as
        -- Define cursor
        cursor software_summary is
            select 
                s.name as software_name,
                count(distinct d.ds_id) as use_count
            from dataset d, analyzes a, software s, keywords k, subjects su
            where d.ds_id = a.ds_id
                and a.sw_id = s.sw_id
                and d.ds_id = k.ds_id
                and d.ds_id = su.ds_id
                and (keyword_in is null or keyword_in = k.keyword)
                and (subject_in is null or subject_in = su.subject)
            group by s.name
            order by use_count desc;

        -- Declare variables
        input_check int;
        record software_summary%rowtype;
        
    begin 

        -- Make sure keyword_in is valid
        if keyword_in is not null then
            begin
                select count(*)
                into input_check
                from keywords
                where keyword = keyword_in;
            exception
                when no_data_found then
                    raise_application_error(-20001, 'Keyword not found.');
            end;
        end if;

        -- Make sure subject_in is valid
        if subject_in is not null then
            begin
                select count(*)
                into input_check
                from subjects
                where subject = subject_in;
            exception
                when no_data_found then
                    raise_application_error(-20002, 'Subject not found.');
            end;
        end if;
            
        -- Print from cursor
        begin
            open software_summary;
            fetch software_summary into record;

            -- If no records returned, throw error
            if not software_summary%found then
                raise_application_error(-20003, 'No records found.');
            end if;

            -- If records found, loop and print output
            while software_summary%found loop
                
                dbms_output.put_line(
                    record.software_name || ': ' || record.use_count
                );
                fetch software_summary into record;
                
            end loop;
            close software_summary;
        end;
    end;


    -- p7. softwareLicenses ----------------------------------

    procedure softwareLicenses (
        keyword_in varchar default null,
        subject_in varchar default null
    ) as
        cursor license_table is
            select 
                sl.name as license_name, 
                count(distinct d.ds_id) as use_count
            from dataset d, software_license sl, keywords k, subjects s
            where d.sw_lic_ID = sl.sw_lic_ID
                and d.ds_id = k.ds_id
                and d.ds_id = s.ds_id
                and (keyword_in is null or keyword_in = k.keyword)
                and (subject_in is null or subject_in = s.subject)
            group by sl.name
            order by use_count desc;

        -- Declare variables
        input_check int;
        record license_table%rowtype;

    begin
        -- Make sure keyword_in is valid
        if keyword_in is not null then
            begin
                select count(*)
                into input_check
                from keywords
                where keyword = keyword_in;
            exception
                when no_data_found then
                    raise_application_error(-20001, 'Keyword not found.');
            end;
        end if;
        
        -- Make sure subject_in is valid
        if subject_in is not null then
            begin
                select count(*)
                into input_check
                from subjects
                where subject = subject_in;
            exception
                when no_data_found then
                    raise_application_error(-20002, 'Subject not found.');
            end;
        end if;

        -- Print from cursor
        begin
            open license_table;
            fetch license_table into record;

            -- If no records returned, throw error
            if not license_table%found then
                raise_application_error(-20003, 'No records found.');
            end if;

            -- If records found, loop and print output
            while license_table%found loop
                
                dbms_output.put_line(
                    record.license_name || ': ' || record.use_count
                );
                fetch license_table into record;
                
            end loop;
            close license_table;
        end;
    end;


    -- p8. funderSummary ----------------------------------------

    procedure funderSummary (
        agency_name_in in varchar default null,
        subject_in in varchar default null
    ) as
        -- Declare cursor
        cursor agency_table is
            select 
                fa.name as name,
                count(distinct p.pub_id) as pub_count,
                sum(d.file_downloads) as downloads,
                sum(g.amount) as total_funded 
            from 
                dataset d, 
                funds f, 
                funding_agency fa, 
                subjects s,
                publication p,
                grants g
            where 
                d.ds_id = s.ds_id
                and d.ds_id = f.ds_id
                and f.agency_id = fa.agency_id
                and d.ds_id = p.ds_id
                and g.grant_id = f.grant_id
                and (agency_name_in is null or agency_name_in = fa.name)
                and (subject_in is null or subject_in = s.subject)
            group by fa.name
            order by fa.name;

        -- Declare variables
        input_check int;
        record agency_table%rowtype;

    begin

        -- Make sure agency name in is valid
        if agency_name_in is not null then
            begin
                select count(*)
                into input_check
                from funding_agency
                where name = agency_name_in;
            exception
                when no_data_found then
                    raise_application_error(-20001, 'Agency not found.');
            end;
        end if;

        -- Make sure subject_in is valid
        if subject_in is not null then
            begin
                select count(*)
                into input_check
                from subjects
                where subject = subject_in;
            exception
                when no_data_found then
                    raise_application_error(-20002, 'Subject not found.');
            end;
        end if;

        -- Print from cursor
        begin 
            open agency_table;
            fetch agency_table into record;

            -- If no records returned, throw error
            if not agency_table%found then
                raise_application_error(-20003, 'No records found.');
            end if;

            -- If records found, loop and print output
            while agency_table%found loop
                
                dbms_output.put_line(
                    record.name || ': ' || 
                    'Publications - ' || record.pub_count ||
                    '. Downloads - ' || record.downloads ||
                    '. Total Funded -  ' || record.total_funded || '.'
                );
                fetch agency_table into record;
                
            end loop;
            close agency_table;
        end;

    end;
   

    -- p9: searchAuthors --------------------------------------

    procedure searchAuthors (search_term varchar) as
        -- Define cursor
        cursor author_matches is
            select ru.name as name
            from users u, registered_user ru, author a
            where u.u_id = ru.u_id
                and ru.ru_id = a.ru_id
                and lower(ru.name) like '%' || lower(search_term) || '%';

        -- Define variables
        record author_matches%rowtype;
    
    begin
        -- Open cursor, fetch into record
        open author_matches;
        fetch author_matches into record;

        -- If no records returned, throw error
        if not author_matches%found then
            raise_application_error(-20001, 'No records found.');
        end if;

        -- If records found, loop and print
        while author_matches%found loop
            dbms_output.put_line(record.name); 
            fetch author_matches into record;

        -- Close loop and cursor
        end loop;
        close author_matches;

    end;


    -- f1. fundingDistribution --------------------------------

    function fundingDistribution(
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

        -- Dynamic query. If 0, return an error. (agency does not fund in that col)
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


    -- f5. datasetActivity ---------------------------------------

    function datasetActivity(
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


    -- f6. proportionGPL -----------------------------------

    function proportionGPL(
    	subject_in in varchar default null
    ) return number
    as
    	dummy int := 0;
    	prop_gpl number(4, 2);
    begin
	
        -- If no input, get prop for whole database
        if subject_in is null then
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
                into dummy
                from subjects
                where subject = subject_in;
            exception 
                when no_data_found then
                    dummy := 0;
            end;

            -- If dummy is 1 (subject exists), run function and return
            if dummy >= 1 then
                select sum(case when sw.name = 'GPL-3.0' then 1 else 0 end) / count(*)
                into prop_gpl
                from software_license sw, dataset d, subjects s
                where s.ds_ID = d.ds_ID
                    and sw.sw_lic_ID = d.sw_lic_ID
                    and subject_in = s.subject;
                return prop_gpl;

            -- If dummy is 0, (no subject), throw error
            elsif dummy = 0 then	
                raise_application_error(
                    -20001, 
                    'Subject not found in Dataverse.'
                );
            end if;

        end if;
    end;


    -- 7. fundingImpact --------------------------------------

    function fundingImpact(
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

        -- Execute query
        execute immediate query into result;
        return result;

    end;


    -- f8. totalSize -------------------------------------------------

    function totalSize(
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


end dataverse;
/
    
show errors        

commit;
