#' Read in a DataShop Student-Step rollup file
#'
#' @param file.name The name of the file to read in.
ds.read <- function(file.name) {
  return(read.csv(file = file.name,sep="\t",check.names=FALSE))
}