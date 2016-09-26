#!/bin/bash
for i in {1..2}
do
  sbatch --export arg1=$i run_regression.cmd
done

