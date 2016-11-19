#!/bin/bash
# parallel job using 1 processor and runs for 8 hours (max)
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -t 6:00:00
#SBATCH --mem 40000
# Execute commands

R CMD BATCH ~/CPAC_script.git/test.R
