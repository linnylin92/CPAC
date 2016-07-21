#!/bin/bash

cd "$1"
files=`ls `     

for subject in $files 

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject  -o /dev/null -e /dev/null runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject  -o /dev/null -e /dev/null runCPAC.cmd
    fi 
done

