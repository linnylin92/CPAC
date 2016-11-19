#!/bin/bash
# parallel job using 1 processor and runs for 8 hours (max)
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -t 6:00:00
# Execute commands

LD_LIBRARY_PATH=/tigress/HANLIU/mridata/lib64 PYTHONPATH=$PYTHONPATH:/tigress/HANLIU/mridata/CPAC /usr/bin/python2.7 /tigress/HANLIU/mridata/CPAC/cpac_run.py /tigress/HANLIU/mridata/CPAC/data_config_INDI/data_config_$arg1.yml /tigress/HANLIU/mridata/CPAC/subjectlistoutput/CPAC_subject_list_$arg1.yml
