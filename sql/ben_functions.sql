-- 2. userLoginAttempt(user_ID, password)

-- check if the hash of the provided password matches the
-- hash stored in the registered_user table
-- hashed using SHA-256
CREATE OR REPLACE FUNCTION userLoginAttempt(
    user_ID IN int DEFAULT -1,
    password IN CHAR
) RETURN number
AS
    old_hash CHAR(64);
    new_hash CHAR(64);
    user_count number := 0;
BEGIN
    -- verify that requested user exists
    SELECT COUNT(*)
        INTO user_count
	FROM registered_user
	WHERE ru_ID=user_ID;
    IF user_count != 1 THEN
        RETURN 0; -- No such user exists
    END IF;

    -- hash the password provided to this function
    SELECT STANDARD_HASH(RTRIM(password), 'SHA256')
        INTO new_hash
        FROM dual;
    SELECT pw_hash
    	INTO old_hash
    	FROM registered_user
	WHERE ru_ID=user_ID;
    IF new_hash = old_hash THEN
        RETURN 1; -- passwords match, successful login
    ELSE
        RETURN 0; -- unsuccessful login
    END IF;
END;
/


-- 3. generateNewPassword(user_ID)

-- If the given user exists within registered_user
-- randomly generate a new password that is alphanumeric
-- and 8 characters long. Verify that it does not match the hash
-- of the previous password. Set this new hash within registered_user
-- and return the new password value.
CREATE OR REPLACE FUNCTION generateNewPassword(
    user_ID IN int DEFAULT -1
) RETURN CHAR
AS
    user_count number := 0;
    old_hash CHAR(64);
    new_pw CHAR(8);
    new_pw_hash CHAR(64);
BEGIN
    -- Check for existence of said user
    SELECT COUNT(*)
        INTO user_count
	FROM registered_user
	WHERE ru_ID=user_ID;
    IF user_count <> 1 THEN
        RAISE_APPLICATION_ERROR(-20500, 'User does not exist');
    END IF;

    -- Generate new random password and check against previous hash
    new_pw := DBMS_RANDOM.STRING('x', 8);
    SELECT STANDARD_HASH(RTRIM(new_pw), 'SHA256')
        INTO new_pw_hash
        FROM dual;
    SELECT pw_hash
    	INTO old_hash
    	FROM registered_user
        WHERE ru_ID=user_ID;
    WHILE old_hash = new_pw_hash
    LOOP
        new_pw := DBMS_RANDOM.STRING('x', 8);
	SELECT STANDARD_HASH(RTRIM(new_pw), 'SHA256')
	    INTO new_pw_hash
	    FROM dual;
    END LOOP;
    RETURN new_pw;
END;
/

SHOW ERRORS
