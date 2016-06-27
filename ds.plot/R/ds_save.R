library(ggplot2)

ds.save <- function(plot, name, width=3, height=2.5, dpi=300) {
  ggsave(plot,name,width=width,height=height,dpi=300,units="in")
}
