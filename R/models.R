#' Return a list of the available KC models in a Student-Step file.
#'
#' @param stu.step A Student-Step export from datashop
ds.models <- function(stu.step) {
  x <- names(stu.step)
  m <- regexpr("KC (.*)",x,perl=TRUE)
  mods <- regmatches(x,m)
  return(unlist(lapply(mods, function(s) (substr(s,5,nchar(s)-1)))))
}