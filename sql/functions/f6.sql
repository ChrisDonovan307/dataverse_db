-- 6. proportionGPL

create or replace function proportionGPL(
	subject_in in varchar default null
) return number
as
	dummy int := 0;
	prop_gpl number(4, 2);
begin
	
	-- If no input, get prop for whole database
    if subject_in is null then
		dbms_output.put_line('SUBJECT IN IS NULL');
		select sum(case when sw.name = 'GPL-3.0' then 1 end) / count(*)
		into prop_gpl
		from software_license sw, dataset d
		where sw.sw_lic_ID = d.sw_lic_ID;
		return prop_gpl;

	-- Prop by subject 
	elsif subject_in is not null then

		-- Check whether subject is in subjects table
		begin
    		select count(*) 
    		into DUMMY
			from subjects
    		where subject = subject_in;
		exception 
			when NO_DATA_FOUND then
				DUMMY := 0;
		end;

		-- If DUMMY is 1 (subject exists), run function and return
		if DUMMY >= 1 then
			select sum(case when sw.name = 'GPL-3.0' then 1 else 0 end) / count(*)
			into prop_gpl
			from software_license sw, dataset d, subjects s
			where s.ds_ID = d.ds_ID
				and sw.sw_lic_ID = d.sw_lic_ID
				and subject_in = s.subject;
			return prop_gpl;

		-- If DUMMY is 0, (no subject), throw error
		elsif DUMMY = 0 then	
			raise_application_error(
				-20001, 
				'Subject not found in Dataverse.'
			);
		end if;

	end if;
end;
/

show errors

-- Testing
select proportionGPL(null) from dual;
select proportionGPL() from dual;
-- Should be 0.6
select proportionGPL('Chemistry') from dual; 
-- Should be 0
select proportionGPL('zzzzzzz') from dual;
-- should throw error because it is not a real subject
select proportionGPL('Medicine, Health and Life Sciences') from dual;

-- Reset
drop function proportionGPL;
