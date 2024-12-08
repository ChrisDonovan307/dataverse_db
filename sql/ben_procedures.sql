-- 3.

-- Need to create a secondary table to store the empty files
-- before they are fully deleted.
-- Included here for reference, but should not be executed along with procedures
-- CREATE TABLE empty_files AS SELECT * FROM files;

CREATE OR REPLACE PROCEDURE dumpEmptyFiles
AS
    cursor f IS
        SELECT * FROM files WHERE filesize <= 0;
    f_record f%ROWTYPE;
BEGIN
    OPEN f;
        FETCH f INTO f_record;
	WHILE f%FOUND
	LOOP
	    INSERT INTO empty_files VALUES(f_record.file_ID, f_record.ds_ID, f_record.filetype,
	    f_record.filesize, f_record.title, f_record.description, f_record.downloads, f_record.pub_date);
	    FETCH f INTO f_record;
	END LOOP;
    CLOSE f;
END;
/

SHOW ERRORS
