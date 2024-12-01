-- 8. funderSummary --------------------------

create or replace procedure funderSummary (
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
/

show errors


-- Testing ------------------------------

exec funderSummary(null, null);
exec funderSummary('National Science Foundation', null);
exec funderSummary(null, 'Physics');
exec funderSummary('National Eye Institute', null);
exec funderSummary('National Eye Institute', 'Physics');

-- Reset
drop procedure funderSummary;

