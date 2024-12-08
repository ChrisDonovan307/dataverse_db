-- Triggers ------------------------------------ 


-- 2. adjustColDatasetCount

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


-- Trigger 5: addFunds
-- NOTE: Funding amounts are not included in Dataverse.
-- The values here are fabricated.

create or replace trigger addFunds
after insert on funds
for each row
declare
	NEWAMOUNT int;
begin
	-- Save the new amount of grant into NEWAMOUNT variable
	select amount
	into NEWAMOUNT
	from grants
	where grant_ID = :NEW.grant_ID;

	-- Add the new grant amount to the total_amount of funding_agency
	update funding_agency
	set total_amount = total_amount + NEWAMOUNT
	where agency_ID = :NEW.agency_ID;
end;
/


commit;
