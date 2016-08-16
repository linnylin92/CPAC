#!/bin/bash

cd "$1"
files=`ls `     

for subject in 025-0040000 025-0040001

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject -o ~ -e ~ ~/mark.shellscript/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject -o ~ -e ~ ~/mark.shellscript/runCPAC.cmd
    fi 
done

