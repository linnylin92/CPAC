#!/bin/bash

cd "$1"
files=`ls `     

for subject in 025-0040000 025-0040001

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject -o /dev/null -e /dev/null ~/CPAC_script.git/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject -o /dev/null -e /dev/null ~/CPAC_script.git/runCPAC.cmd
    fi 
done

