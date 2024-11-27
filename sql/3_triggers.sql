-- Triggers --------------------------------------------------


-- 3. adjustDatasetFilesCount

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


-- 4. adjustColFilesCount

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


commit;
