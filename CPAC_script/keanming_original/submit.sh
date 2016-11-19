#!/bin/bash
     
for j in {0050002..0050060}
do
sbatch --export arg1=$j  runsim.cmd
#sbatch --export arg1=$i -o /dev/null -e /dev/null runsim.cmd
done
