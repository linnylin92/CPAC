#! /bin/sh

cd "$1"
files=`ls `

mkdir /home/maksen/data_config/



for subject in $files
do
    echo $subject  
    sed -e "s/50002/$subject/;s/ABIDE/$2/;s/Pitt/$2/" /home/maksen/CPAC_script.git/data_config.yml > /tigress/HANLIU/mridata/CPAC/data_config_INDI/data_config_$subject.yml

done


