#!/bin/bash

cd "$1"
files = `ls "$3"* -d`

mkdir "$2" -p
for subject in files
do
    mkdir "$2"/"$subject" 
    cp "$subject"/functional_mni/_scan_func_rest_1/_csf_threshold_0.98/_gm_threshold_0.7/_wm_threshold_0.98/_compcor_ncomponents_5_selector_pc10.linear1.wm0.global1.motion1.quadratic1.gm0.compcor1.csf1/_bandpass_freqs_0.001.0.08
/bandpassed_demeaned_filtered_antswarp.nii.gz "$2"/"$subject"/func.nii.gz
    cp "$subject"/mni_normalized_anatomical/transform_Warped.nii.gz "$2"/"$subject"/anat.nii.gz
done
    
chmod g+w -R -f "$2"