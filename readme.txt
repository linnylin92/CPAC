#running the CPAC preprocessing pipeline

sh submit.sh /tigress/HANLIU/mridata/CPAC_rawdata/INDI/Retrospective/COBRE/ 1
sh submit.sh /tigress/HANLIU/mridata/CPAC_rawdata/INDI/Retrospective/COBRE/ 2

#preprocessing files

1. submit.sh
- accepts 2 arguments
- argument 1 is file path to directory with only subject names. 
Assumes that all files within this directory are subject names
- argument 2 is an argument that is assumed to be either 1 or 2

If arg2==1, executes runCreateSubjectList.cmd for each subject. 
			sets file permission to make all files readable and editale to others
If arg2==2, executes runCPAC.cmd for each subject, ie runs CPAC preprocessing
			sets file permission to make all files readable and editale to others
for each subject 


2. runtest.cmd

- not sure what this does?

3. runCPAC.cmd

- accepts 1 argument
- argument is subject name

runs CPAC preprocessing code using data config file for the particular subject name

4. runCreateSubjectList.cmd
- accepts 1 argument
- argument is subject names

runs cpac_createSubjectlist.py, which creates a subject list for each subject name 
using a data config file. 

5. data_config.yml

-template data config file
-assumes data is from INDI/Retrospective

6. data_config_rename.sh

- accepts 1 arugument
- argument is file path to directory with only subject names
- created data config for each subject name in current project

7. check.R

- not sure what this does?

#postprocessing files

8. movepreprocessed.sh

- accepts 3 arguments 

