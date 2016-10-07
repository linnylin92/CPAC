#!/bin/bash
for i in {1..2}
do
  sbatch --export arg1=$i -o /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out -e /tigress/HANLIU/mridata/CPAC_logs/slurm.%N.%j.out run_regression.cmd
done

