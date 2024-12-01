-- Indexes ---------------------------------------------------

-- 1. Keywords
create index keyword_index on keywords (keyword);


-- 2. Author names
create index ru_name_index on registered_user (name);


-- 3. Funding agency names
create index agency_index on funding_agency (name);


-- Check
select index_name, table_owner, table_name
from user_indexes
where index_name like '%INDEX';

-- Reset
drop index keyword_index;
drop index ru_name_index;
drop index agency_index;


