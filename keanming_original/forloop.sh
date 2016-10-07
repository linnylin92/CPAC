#!/bin/bash
     
for j in {0050002..0050060}
do

sed -e "s/50002/$j/" data_config_abide.yml > data_config/data_config_abide$j.yml

#sbatch --export arg1=50002 runsim.cmd
#sbatch --export arg1=$i -o /dev/null -e /dev/null runsim.cmd
done
