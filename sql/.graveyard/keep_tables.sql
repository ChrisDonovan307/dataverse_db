BEGIN
   -- Drop all triggers
   FOR rec IN (SELECT trigger_name, table_name FROM user_triggers) LOOP
      EXECUTE IMMEDIATE 'DROP TRIGGER ' || rec.trigger_name;
   END LOOP;

   -- Drop all views
   FOR rec IN (SELECT view_name FROM user_views) LOOP
      EXECUTE IMMEDIATE 'DROP VIEW ' || rec.view_name;
   END LOOP;

   -- Drop all functions
   FOR rec IN (SELECT object_name FROM user_objects WHERE object_type = 'FUNCTION') LOOP
      EXECUTE IMMEDIATE 'DROP FUNCTION ' || rec.object_name;
   END LOOP;

   -- Drop all procedures
   FOR rec IN (SELECT object_name FROM user_objects WHERE object_type = 'PROCEDURE') LOOP
      EXECUTE IMMEDIATE 'DROP PROCEDURE ' || rec.object_name;
   END LOOP;

   -- Drop all packages
   FOR rec IN (SELECT object_name FROM user_objects WHERE object_type = 'PACKAGE') LOOP
      EXECUTE IMMEDIATE 'DROP PACKAGE ' || rec.object_name;
   END LOOP;

   -- Drop all package bodies
   FOR rec IN (SELECT object_name FROM user_objects WHERE object_type = 'PACKAGE BODY') LOOP
      EXECUTE IMMEDIATE 'DROP PACKAGE BODY ' || rec.object_name;
   END LOOP;

   -- Drop all indexes
   FOR rec IN (SELECT index_name, table_name FROM user_indexes) LOOP
      EXECUTE IMMEDIATE 'DROP INDEX ' || rec.index_name;
   END LOOP;

END;
/

