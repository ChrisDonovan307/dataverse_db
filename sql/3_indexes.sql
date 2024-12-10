-- Indexes ---------------------------------------------------

-- 1. Keywords
create index keyword_index on keywords (keyword);


-- 2. Author names
create index ru_name_index on registered_user (name);


-- 3. Funding agency names
create index agency_index on funding_agency (name);

