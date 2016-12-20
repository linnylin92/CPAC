###### STEP 0: read in data ########

case_subj <- c("025-0040046", "025-0040002", "025-0040017")
control_subj <- c("025-0040061", "025-0040090", "025-0040113")

read_subj <- function(vec){
  list <- lapply(vec, function(x){
    dat <- oro.nifti::readNIfTI(paste0("/tigress/HANLIU/mridata/CPAC_finished/INDI/Retrospective/COBRE/",
                                       x, "/func.nii.gz"), reorient = F)
    dat@.Data
  })
  names(list) <- vec
  
  list
}

case_dat <- read_subj(case_subj)
control_dat <- read_subj(control_subj)

###### STEP 1: apply the parcellation #######

parcel <- readNIfTI("C://Mark/College/Senior Year/PACM certificate/AAL_3mm_90parcel.nii.gz")
parcel <- parcel@.Data

convert_data <- function(mat_list){
  lapply(mat_list, function(x){
    cat("\n")
    parcellation(x, parcel)
  })
}

case_dat <- convert_data(case_dat)
control_dat <- convert_data(control_dat)



