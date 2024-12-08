-- Additional trigers. Stored in a separate file to avoid ruining everything

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
