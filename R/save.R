#' Save a ds.plot in a commonly useful png format.
#'
#' This is just a basic wrapper around ggplot's ggsave but the default settings make it perfect for fitting into 
#' a single column of the ACM/EDM paper format.
#'
#' @param plot   The plot data
#' @param name   The desired filename
#' @param width  The width in inches (defaults to 3, which is recommended width for AcM/EDM format)
#' @param height The height in inches (which I default to 2.5)
#' @param dpi    The dpi to output at (defaults to 300)
#' @export
ds.save <- function(plot, name, width=3.3, height=2.08, dpi=300) {
  ggplot2::ggsave(name,plot=plot,width=width,height=height,dpi=dpi,units="in")
}