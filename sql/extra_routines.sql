CREATE OR REPLACE TYPE strings_t IS TABLE OF VARCHAR2 (100)
/

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

-- In the final hour something related to the merging of all project files
-- has bumped this function the wrong way.
-- The function itself will compile without error, but will crash when attempting
-- to run. Error is believed to be related to strings_t table type.
CREATE OR REPLACE FUNCTION search(
    searchString IN VARCHAR
) RETURN strings_t
AS
    match_count int;
    start_index NUMBER := 1;
    end_index NUMBER;
    item VARCHAR2(255);
    -- default value is empty collection
    items strings_t := strings_t();
BEGIN
    LOOP
        end_index := INSTR(searchString, ',', start_index);
        IF end_index = 0 THEN
            item := SUBSTR(searchString, start_index);
	    -- repeated code to grab the last item
	    FOR t IN (SELECT owner, table_name, column_name
	        FROM all_tab_columns
		WHERE owner <> 'SYS' AND data_type LIKE '%CHAR%')
		LOOP

		EXECUTE IMMEDIATE
		'SELECT COUNT(*) FROM ' || t.owner || '.' || t.table_name ||
		' WHERE ' || t.column_name || ' LIKE ''%' || item || '%'''
		INTO match_count;

		IF match_count > 0 THEN
		   items.EXTEND;
		   items(items.LAST) := '' || match_count || ' matches for ' || item ||
		   ' found in ' || t.table_name || '.' || t.column_name || '';
		   match_count := 0;
		END IF;
		END LOOP;
            RETURN items;
        END IF;
        
        item := SUBSTR(searchString, start_index, end_index - start_index);
        -- Process item here
	-- Find all non-system table.column with text datatype
		FOR t IN (SELECT owner, table_name, column_name
			FROM all_tab_columns
			WHERE owner <> 'SYS' AND data_type LIKE '%CHAR%')
			LOOP

			-- Select tuples that contain the keyword within a text field
			EXECUTE IMMEDIATE 
			'SELECT COUNT(*) FROM ' || t.owner || '.' ||  t.table_name ||
			' WHERE ' || t.column_name || ' LIKE ''%' || item || '%'''
			INTO match_count;

			-- When we find match tuples, append them to the items text table
			IF match_count > 0 THEN
				items.EXTEND;
				items(items.LAST) := '' || match_count || ' matches for ' || item ||
				' found in ' || t.table_name || '.' || t.column_name || '';
				match_count := 0;
			END IF;
			END LOOP;

	--items.EXTEND;
	--items(items.LAST) := REPLACE(REGEXP_REPLACE(item, '\s'), CHR(0));
        
        start_index := end_index + 1;
    END LOOP;
    RETURN items;
END;
/

SHOW ERRORS
