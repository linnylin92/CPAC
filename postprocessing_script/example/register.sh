#TO USE:
#First argument: preprocessed anatomical brain (nii file)
#Second argument: preprocessed functional brain (nii file)
#Third argument: MNI image (nii file)

#FUNC -> T1
flirt -ref $1 -in $2 -out func2anat -omat func2anat.mat -cost corratio -dof 6 -interp trilinear
convert_xfm -inverse -omat anat2func.mat func2anat.mat

#T1 -> STANDARD
flirt -ref $3 -in $1 -out anat2mni -omat anat2mni.mat -cost corratio -searchcost corratio -dof 12 -interp trilinear
convert_xfm -inverse -omat mni2anat.mat anat2mni.mat

#FUNC -> STANDARD
convert_xfm -omat func2mni.mat -concat anat2mni.mat func2anat.mat
flirt -ref $3 -in $2 -out func2mni -applyxfm -init func2mni.mat -interp trilinear
convert_xfm -inverse -omat mni2func.mat func2mni.mat

