#!/bin/bash

location=`pwd`

cd "$1"
rm *.out
rm *.log
files=`ls `     

for subject in 025-0040000 025-0040001 025-0040002 025-0040003

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject $location/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject $location/runCPAC.cmd
    fi 
done

