-- Scheduled job to use funderSummary procedure on a recurring basis

-- Create job
begin
    dbms_scheduler.create_job(
        job_name        => 'minutelyFunderSummary',
        job_type        => 'stored_procedure',
        job_action      => 'BEGIN funderSummary(null, null); END;',
        start_date      => systimestamp,
        repeat_interval => 'freq=minutely; interval=1',
        enabled         => true
    );
end;
/

-- Create a schedule
--begin
--    dbms_scheduler.create_schedule(
--        schedule_name   => 'minutely_schedule',
--        start_date      => systimestamp,
--        repeat_interval

-- Create a table to log results into
create table job_log (
    job_log_id int primary key,
    timestamp TIMESTAMP,
    message varchar(5000)
);
/

-- Run it manually
begin
    dbms_scheduler.run_job('minutelyFunderSummary')
-- Reset
begin
    dbms_scheduler.drop_job('minutelyFunderSummary');
end;
/

drop table job_log;
