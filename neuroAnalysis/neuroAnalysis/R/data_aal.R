#' Automated Anatomical Labeling in 3mm
#'
#' @name AAL_3mm
#' @docType data
#' @format A BrcParcellation object of size 61 x 73 x 61 voxels, of which
#' there are 47,636 voxels in representing the brain (17.5%). The parcellation is
#' not symmetric.
#' \describe{
#'   \item{dim3d}{A 3-element vector for the 3D dimensions}
#'   \item{partition}{A vector of length prod(dim3d) having values 0,...,n
#'   where n is the total number of parcellations}
#' }
#' @author Kevin Lin \email{kevinl1@andrew.cmu.edu}
#' @source TO BE FILLED
#' @keywords data
NULL

#' Automated Anatomical Labeling Table
#'
#' @name AAL_tab
#' @docType data
#' @format A table with 116 rows and 3 variables
#' \describe{
#'   \item{Index}{the value used in brcdata to encode the partitions for AAL}
#'   \item{Short.Name}{short name of the scientific name for the partition}
#'   \item{Full.Name}{full name of the scientific name for the partition. Only the first
#'   90 regions are recorded.}
#' }
#' @author Kevin Lin \email{kevinl1@andrew.cmu.edu}
#' @source TO BE FILLED
#' @keywords data
NULL
