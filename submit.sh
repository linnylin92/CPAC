#!/bin/bash

location=`pwd`

cd "$1"
rm *.out
rm *.log
files=`ls `     
if [ -z "$1" ]
    then
      echo "First argument (file path to directory with only subjects) not supplied"
fi
if [ -z "$2" ]
    then
      echo "Second argument (argument 1 or 2 for createSubjectList or runCPAC) not supplied"
fi

for subject in 025-0040000 025-0040001
do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out $location/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out $location/runCPAC.cmd
    fi 
done

