#!/bin/bash

location=`pwd`

cd "$1"
files=`ls `     

for subject in $files

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject $location/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject $location/runCPAC.cmd
    fi 
done

