-- Triggers ------------------------------------ 

-- 1 hashUserPassword
-- Grab the entered password, hash using SHA-256, and store the hash value

CREATE OR REPLACE TRIGGER hashUserPassword
	BEFORE INSERT ON registered_user
	FOR EACH ROW
BEGIN
	SELECT STANDARD_HASH(RTRIM(:new.pw_hash), 'SHA256')
	INTO   :new.pw_hash
	FROM   dual;
END;
/

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

-- 6a. fileDownloadTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER fileDownloadTimestamp
    BEFORE INSERT ON file_download
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

-- 6b. datasetDownloadTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER datasetDownloadTimestamp
    BEFORE INSERT ON dataset_download
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

-- 7a. fileUploadTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER fileUploadTimestamp
    BEFORE INSERT ON file_upload
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

-- 7b. datasetUploadTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER datasetUploadTimestamp
    BEFORE INSERT ON dataset_upload
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

-- 8a. manageCollectionTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER manageCollectionTimestamp
    BEFORE INSERT ON manage_collection
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

-- 8b. manageDataverseTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER manageDataverseTimestamp
    BEFORE INSERT ON manage_dataverse
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

-- 9. contactTimestamp
-- On INSERT of a new tuple, capture and store current datetime

CREATE OR REPLACE TRIGGER contactTimestamp
    BEFORE INSERT ON contact
    FOR EACH ROW
DECLARE
    n_timestamp timestamp;
BEGIN
    SELECT localtimestamp INTO n_timestamp FROM dual;
    :new.timestamp := n_timestamp;
END;
/

COMMIT;
