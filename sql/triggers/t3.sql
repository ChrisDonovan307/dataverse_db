-- 2. adjustDatasetFilesCount

create or replace trigger adjustDatasetFilesCount
after insert or delete on files
for each row
begin
	if inserting then
		update dataset
		set n_files = n_files + 1
		where ds_ID = :NEW.ds_ID;
	elsif deleting then
		update dataset
		set n_files = n_files - 1
		where ds_ID = :OLD.ds_ID;
	end if;
end;
/

show errors


-- savepoint for testing
-- savepoint before_insert;

-- check number of files in a certain dataset
select n_files
from dataset
where ds_ID = 'doi:10.7281/T1/6Y08LS';

-- insert into files
insert into files values ('test', 'doi:10.7281/T1/6Y08LS', null, null, null, null, null, null);

-- See if it worked
select n_files
from dataset
where ds_ID = 'doi:10.7281/T1/6Y08LS';

-- delete from files
delete from files
where file_ID = 'test';

-- See if it worked
select n_files
from dataset
where ds_ID = 'doi:10.7281/T1/6Y08LS';

-- Reset everything
rollback;
drop trigger adjustDatasetFilesCount;
commit;



