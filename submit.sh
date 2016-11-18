#!/bin/bash

location=`pwd`

#check arguments
cd "$1"
files=`ls `     
if [ -z "$1" ]
    then
      echo "ERROR: First argument (file path to directory with only subjects) not supplied"
      exit
fi
if [ -z "$2" ]
    then
      echo "ERROR: Second argument (argument 1 or 2 for createSubjectList or runCPAC) not supplied"
      exit
fi

#run commands
for subject in $files

do
    if [ "$2" = "1" ]
    then
	sbatch --export arg1=$subject -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out $location/runCreateSubjectList.cmd
    else 
	sbatch --export arg1=$subject -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out $location/runCPAC.cmd
    fi 
done

#set file permissions correctly
if [ "$2" = "1" ]
then
    chmod g+w /tigress/HANLIU/mridata/CPAC/subjectlistoutput -R -f
    cd "$1"
    rm extract_data_logs.log
else
    chmod g+w /tigress/HANLIU/mridata/CPAC_processed/tmp_finished_INDI -R -f
fi
chmod g+w /tigress/HANLIU/mridata/CPAC_logs -R -f
