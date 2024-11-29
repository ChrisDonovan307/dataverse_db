-- Trigger 2: adjustColDatasetCount

create or replace trigger adjustColDatasetCount
after insert or delete on dataset
for each row
begin
	if inserting then
		update collection
		set n_datasets  = n_datasets + 1
		where col_ID = :NEW.col_ID; 

	elsif deleting then
		update collection
		set n_datasets  = n_datasets - 1
		where col_ID = :OLD.col_ID;
	end if;
end;
/

show errors


-- check number of datasets in a certain collection
select n_datasets
from collection
where col_ID = 1;

-- insert into files
insert into dataset values (
	'test',
	(select col_ID from dataset where col_ID = 1 fetch first 1 row only),
	null, 
	null, 
	null, 
	null, 
	null,
        null,	
	null,
	null
);


-- See if it worked
select n_datasets
from collection
where col_ID = 1;

-- delete from files
delete from dataset
where ds_ID = 'test';

-- See if it worked
select n_datasets
from collection
where col_ID = 1;

-- Reset everything
rollback;
drop trigger adjustColDatasetCount;

