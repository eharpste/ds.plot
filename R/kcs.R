#' List the KCs available within a partiuclar KC model of a DataShop student-step export
#' 
#' @param stu.step A DataShop Student-Step export data.frame
#' @param model    The name of a KC model within the dataset
#' @export
ds.kcs <- function(stu.step, model) {
  mod.name <- paste0("KC (",model,")")
  if(mod.name %in% names(stu.step)) {
    ds <- stu.step
    ds$knowledge_component <- as.factor(ds[[mod.name]])
    return(unique(unlist(Map(function(x) strsplit(toString(x), "~~"), ds$knowledge_component))))
  }
  else {
    warning(paste0('KC model "',model,'" not found in stu.step'))
    return(list())
  }
}