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
      echo "ERROR: Second argument (file path to output directory) not supplied"
      exit
fi

#run commands
for subject in $files
do
    mkdir "$2"/"$subject"
    cd /tigress/HANLIU/mridata/CPAC_logs/
    sbatch --export arg1=$subject --export arg2=$1 --export arg3=$2 -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out $location/analysis.cmd
done

#set file permissions correctly
chmod g+w /tigress/HANLIU/mridata/CPAC_subjectgraphs -R -f
chmod g+w /tigress/HANLIU/mridata/CPAC_logs -R -f
