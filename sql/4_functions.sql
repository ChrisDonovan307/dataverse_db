
-- 8. totalSize(scope, id) ---------------------------------------

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


