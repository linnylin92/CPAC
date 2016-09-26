#!/bin/bash
# parallel job using 1 processor and runs for 8 hours (max)
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -t 6:00:00
#SBATCH --mem 40000
# Execute commands

#python --version
LD_LIBRARY_PATH=/tigress/HANLIU/mridata/lib64 PYTHONPATH=$PYTHONPATH:/tigressdata/HANLIU/mridata/CPAC /usr/bin/python2.7 cpac_createSubjectlist.py data_config_INDI/data_config_$arg1.yml
