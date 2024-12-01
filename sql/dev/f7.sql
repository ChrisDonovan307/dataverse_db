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

show errors

-- Test
select fundingImpact(1, null, 'publications') from dual;
select fundingImpact(1, null, 'downloads') from dual;

-- Explore NSF with different combinations of inputs
select fundingImpact(25, null, 'downloads') from dual;
select fundingImpact(null, 'National Science Foundation', 'downloads') from dual;
select fundingImpact(25, 'National Science Foundation', 'downloads') from dual;

-- Try NSF with a wrong grant ID number. Should throw error.
select fundingImpact(1, 'National Science Foundation', 'downloads') from dual;

-- Reset
drop function fundingImpact;


