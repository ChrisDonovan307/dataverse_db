-- Package --------------------------------------------------------

create or replace package dataverse as

    -- Procedure: use a string to search for authors in dataverse
    procedure search_authors (search_term varchar);

end dataverse;
/


-- Body -----------------------------------------------------------

create or replace package body dataverse as
    
    -- Procedure: search_authors
    procedure search_authors (search_term varchar) as
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
end dataverse;
/
    
show errors        
           
 
-- Check -----------------------------------------------------

select object_name, status 
from user_objects 
where object_type = 'package';

-- Author search
begin
    dataverse.search_authors('david');
end;
/


-- Reset

drop package dataverse;

