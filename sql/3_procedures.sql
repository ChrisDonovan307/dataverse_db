-- 6. softwareUse ---------------------------------------------

create or replace procedure softwareUse (
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
/
