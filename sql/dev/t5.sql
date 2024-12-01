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

show errors

-- check total_amount from a certain agency_id in funding_agency table
select total_amount
from funding_agency
where agency_ID = 4;

-- add a record to grant table, then another to funds table
insert into grants values (
	999,
	'test',
	42
);

insert into funds values (
	999,
	'doi:10.7281/T10Z715B',
	4
);

-- check total_amount for agency_ID 1 again
select total_amount
from funding_agency
where agency_ID = 4;

-- reset
rollback;
drop trigger addFunds;

