-- p3.

-- Need to create a secondary table to store the empty files
-- before they are fully deleted.
-- Included here for reference, but should not be executed along with procedures
-- CREATE TABLE empty_files AS SELECT * FROM files;

CREATE OR REPLACE PROCEDURE dumpEmptyFiles
AS
    CURSOR f IS
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


-- p4

-- Search through the keywords within a dataset and generate a summary
-- table listing each keyword by its count (descending)

--CREATE TABLE keyword_count (keyword VARCHAR(100), count_of number);

CREATE OR REPLACE PROCEDURE keywordSummary(
    dataset in VARCHAR
)
AS
    CURSOR kw IS
        SELECT keyword FROM keywords
	    WHERE ds_ID = dataset;
    kw_record kw%ROWTYPE;
    kw_count number;
    kw_rec_count number;
BEGIN
    OPEN kw;
        FETCH kw INTO kw_record;
	WHILE kw%FOUND
	LOOP
	    SELECT COUNT(*)
	        INTO kw_count
	        FROM keywords
	        WHERE keyword=kw_record.keyword;
	    -- Has this keyword already been counted?
            SELECT COUNT(*)
	        INTO kw_rec_count
	        FROM keyword_count
	        WHERE keyword=kw_record.keyword;
            IF kw_rec_count > 0 THEN
		INSERT INTO keyword_count VALUES(kw_record.keyword, kw_count);
	    -- Is the current count consistent with records?
	    ELSE
	        SELECT count_of
		    INTO kw_rec_count
		    FROM keyword_count
	            WHERE keyword=kw_record.keyword;
		IF kw_count != kw_rec_count THEN
		    UPDATE keyword_count SET count_of=kw_count
		        WHERE keyword=kw_record.keyword;
		END IF;
            END IF;
	    FETCH kw INTO kw_record;
	END LOOP;
    CLOSE kw;
END;
/

SHOW ERRORS
