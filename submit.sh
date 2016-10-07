#!/bin/bash

cd "$1"
files=`ls `     

for subject in 025-0040000 025-0040001

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject ~/mark.shellscript/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject ~/mark.shellscript/runCPAC.cmd
    fi 
done

