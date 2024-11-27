-- Trigger 2: adjustColFilesCount

create or replace trigger adjustColFilesCount
after insert or delete on files
for each row
declare
	CID int; 
begin
	if inserting then
		-- get col_ID of dataset affected, save as CID
		select col_ID
		into CID
		from dataset
		where ds_ID = :NEW.ds_ID;

		-- add to n_files count of collection
		update collection
		set n_files = n_files + 1
		where col_ID = CID; 

	elsif deleting then
		-- get col_ID of dataset affected, save as CID
		select col_ID
		into CID
		from dataset
		where ds_ID = :OLD.ds_ID;
		
		-- subtract from n_files count of collection
		update collection
		set n_files = n_files - 1
		where col_ID = CID;
	end if;
end;
/

show errors


-- check number of files in a certain collection
select n_files
from collection
where col_ID = 1;

-- insert into files
insert into files values (
	'test',
	(select ds_ID from dataset where col_ID = 1 fetch first 1 row only),
	null, 
	null, 
	null, 
	null, 
	null, 
	null
);


-- See if it worked
select n_files
from collection
where col_ID = 1;

-- delete from files
delete from files
where file_ID = 'test';

-- See if it worked
select n_files
from collection
where col_ID = 1;

-- Reset everything
rollback;
drop trigger adjustColFilesCount;

