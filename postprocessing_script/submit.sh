#!/bin/bash

location=`pwd`

#check arguments
cd "$1"
files=`ls `     
if [ -z "$1" ]
    then
      echo "ERROR: First argument (file path to directory with only subjects) not supplied"
      exit

#run commands
for subject in $files

do
	mkdir "$2"/"$subject"
    cd /tigress/HANLIU/mridata/CPAC_logs/
	sbatch --export arg1=$subject arg2=$1 arg3=$2 -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out $location/analysis.cmd
done

#set file permissions correctly
chmod g+w /tigress/HANLIU/mridata/CPAC_subjectgraphs -R -f
chmod g+w /tigress/HANLIU/mridata/CPAC_logs -R -f
