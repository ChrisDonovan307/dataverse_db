-- 5. datasetActivity

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

show errors

-- Test. Note that all downloads in log are after Nov 1st 2024.
select datasetActivity() from dual;
select datasetActivity('doi:10.7281/T10Z715B') from dual;
select datasetActivity('doi:10.7281/T1/6BHBC4') from dual;
select datasetActivity( 'doi:10.7281/T1/6BHBC4', '01-NOV-24', '15-NOV-24') from dual;
select datasetActivity( 'doi:10.7281/T1/6BHBC4', '01-NOV-24') from dual;
select datasetActivity( 'doi:10.7281/T1/6BHBC4', null, '01-NOV-24') from dual;
-- this should return no downloads because it is only up to Nov 1st 2024

-- Reset
drop function datasetActivity;



