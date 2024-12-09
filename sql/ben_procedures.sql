-- p1

-- find all publications that list a given author
-- can be accomplished in a single SELECT statement but is included
-- here as a procedure to ensure continuity with design doc
CREATE OR REPLACE PROCEDURE findPubsByAuthor(
    authorName IN VARCHAR
)
AS
    CURSOR pub IS
        SELECT pub_ID, citation, url
	FROM publication
	WHERE citation LIKE '%' || authorName || '%''';
    pub_record pub%ROWTYPE;
BEGIN
    OPEN pub;
    FETCH pub INTO pub_record;
    WHILE pub%FOUND
    LOOP
        DBMS_OUTPUT.PUT_LINE(authorName || ' contributed to, ' || pub_record.pub_ID || ', '
	|| pub_record.url || ', ' || pub_record.citation);
    END LOOP;
    CLOSE pub;
END;
/

SHOW ERRORS

-- p2

-- Find all datasets that list a specified author and display information
-- on those datasets in the form ds_ID, title, and url
CREATE OR REPLACE PROCEDURE findDataSetsByAuthor(
    authorName IN VARCHAR
)
AS
    CURSOR ds IS
        SELECT d.ds_ID, d.title, d.url 
	FROM publication p, dataset d
	WHERE citation LIKE '%' || authorName || '%'''
	    AND p.ds_ID = d.ds_ID;
    ds_record ds%ROWTYPE;
BEGIN
    OPEN ds;
    FETCH ds INTO ds_record;
    WHILE ds%FOUND
    LOOP
        DBMS_OUTPUT.PUT_LINE(authorName || ' contributed to, ' || ds_record.ds_ID || ', '
	|| ds_record.title || ', ' || ds_record.url);
	FETCH ds INTO ds_record;
    END LOOP;
    CLOSE ds;
END;
/

SHOW ERRORS

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

SHOW ERRORS

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
            IF kw_rec_count = 0 THEN
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

-- p5
-- Accepts a single keyword string and a list of alias strings
-- Consolidate all instances of aliases within the keywords table
-- to exactly match the provided keyword
CREATE OR REPLACE PROCEDURE cleanKeywords(
    kw IN VARCHAR,
    aliases IN strings_t
)
AS
    CURSOR ds IS
        SELECT ds_ID FROM keywords;
    ds_record ds%ROWTYPE;
    match_count number;
    a_record VARCHAR(100);
BEGIN
    -- check all alias strings
    FOR i IN aliases.FIRST..aliases.LAST
    LOOP
        a_record := aliases(i);
	-- check for each dataset to prevent duplicate primary keys
        OPEN ds;
	FETCH ds INTO ds_record;
	WHILE ds%FOUND
	LOOP
	    SELECT COUNT(*)
	        INTO match_count
		FROM keywords
		WHERE keyword = kw AND ds_ID = ds_record.ds_ID;
            -- If this dataset already contains a reference to the desired keyword
	    -- then simply delete the record containing this alias
	    IF match_count > 0 THEN
                DELETE FROM keywords WHERE keyword = a_record;
	    -- otherwise update the record so that alias becomes kw
	    ELSE
	        UPDATE keywords
		SET keyword = kw
		WHERE keyword = a_record;
	    END IF;
	    FETCH ds INTO ds_record;
	END LOOP;
	CLOSE ds;
    END LOOP;
END;
/

SHOW ERRORS
