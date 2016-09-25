#!/bin/bash

    for i in {1..2}

    do

sbatch --export arg1=100,arg2=10,arg3=$i runscenario1.cmd

done

