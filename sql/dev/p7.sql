-- 7. softwareLicenses ----------------------------------

create or replace procedure softwareLicenses (
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
/

show errors

-- Test
exec softwareLicenses(null, null);
exec softwareLicenses('COVID-19', null);
exec softwareLicenses(null, 'Social Sciences');
exec softwareLicenses('COVID-19', 'Physics');

-- Reset
drop procedure softwareLicenses;


