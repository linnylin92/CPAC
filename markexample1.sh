#! /bin/sh

cd "$1"
files=`ls `

for subject in $files
do
    echo $subject  
done


