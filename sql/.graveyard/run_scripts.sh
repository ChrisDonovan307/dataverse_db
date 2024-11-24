#!/bin/bash

# Set the Oracle environment variables if needed

# export ORACLE_HOME=/path/to/oracle_home
# export PATH=$ORACLE_HOME/bin:$PATH
# export ORACLE_SID=your_database_sid

# Oracle credentials
USER="$1"
PASSWORD="$2"
DB="$3"

# Run all scripts
for SCRIPT in *.sql; do
	echo "Running $SCRIPT..."
	sqlplus -s "$USER/$PASSWORD@$DB" <<EOF
	

    @${SCRIPT}
    EXIT;
EOF
done



