Hi Byung! We have organized our project into a set of sequential scripts that can be run using @run (same as the run_script file that you shared with the class).

1_tables.sql - All Create Table statements and column constraint specification. Also contains the clustered index. 

2_insert.sql - Insert statements generated from the data Chris pulled from the Dataverse API. This will take ~30-60 sec to run. 

3_indexes.sql – B* tree indexes on select columns. 

4_triggers.sql - All specified triggers that will run during the lifetime of this database. Needs to be built after inserting tuples or timestamp data will be overwritten. 
5_package.sql - Container for the functions and procedures available within our database.  

To facilitate the execution of this project we have created a run_all.sql script (@run_all) that will sequentially execute individual sql files as necessary to build the database. This is modified version of the @run_script file, which saves all outputs into a single run_scipt.out file. 

After execution of the run_all.sql script, the database will be live and you may run queries against our tables. We have also included a check.sql file containing queries and notes for how we verified the correctness of each function and procedure. Lastly, the file drop.sql should clean up everything when you are done. Note that the drop.sql file is a utility function that we found and adapted, but did not write ourselves.  
