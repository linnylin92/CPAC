#!/bin/bash

LD_LIBRARY_PATH=/tigress/HANLIU/mridata/lib64 PYTHONPATH=$PYTHONPATH:/tigressdata/HANLIU/mridata/CPAC /usr/bin/python2.7 cpac_createSubjectlist.py data_config_INDI/data_config_$arg1.yml
