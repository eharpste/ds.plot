library(ggplot2)

#' A Smooth Plot DataShop Learning Curves
#'
#' Creates a loess smoothed ggplot from a student step roll up output from Datashop.
#'
#' @param stu.step    A data.frame of a direct load of a student-step rollup file.
#' @param model       The name of a KC model to use as it appears in DataShop
#' @param kc          The name of a particular Kc to get individual kc curves or its number in the list of kcs
#' @param title       A title for the plot
#' @param line.type   Controls whether to render a "smooth" loess line or "discrete" true averages line.
#' @param label.scale A scaling factor for the axis label and title text.
#' @keywords ds.plot
#' @export
ds.plot <- function(stu.step, model, kc="all", line.type="smooth", title=NULL, legend.position="none", label.scale=1){
  line.type <- tolower(line.type)
  mod.name <- paste0("KC (",model,")")
  opp.name <- paste0("Opportunity (",model,")")
  pred.name <- paste0("Predicted Error Rate (",model,")")

  ds <- stu.step
  ds$knowledge_component <- as.factor(ds[[mod.name]])
  ds$opportunity <- ds[[opp.name]]
  ds$afm.predict <- ds[[pred.name]]
  ds$actual <- 1
  ds$actual[ds[["First Attempt"]] == "correct"] <- 0

  if (kc == "all" || kc < 1) {
    opp <- ds$opportunity
    y <- ds$actual
    afm.std <- ds$afm.predict
    kc.name = "all"
  }
  else {
    kcs <- unique(unlist(Map(function(x) strsplit(toString(x), "~~"), ds$knowledge_component)))
    if (class(kc) == "character"){
      kc.name <- kc
      kc <- match(kc,kcs)
    }
    else {
      kc.name <- kcs[kc]
    }
    ds$kcid <- match(ds$knowledge_component, kcs)
    opp <- ds$opportunity[ds$kcid == kc]
    y <- ds$actual[ds$kcid == kc]
    afm.std <- ds$afm.predict[ds$kcid == kc]
  }
  error <- c(y, afm.std)
  label <- as.factor(c(rep("Actual", length(y)), rep("AFM", length(afm.std))))
  plotData = data.frame(opp, error, label)

  if(is.null(title)) {
    if(kc.name == "all") {
      title = model
    }
    else {
      title = kc.name
    }
  }

  if(substring('discrete', 1, nchar(line.type)) == line.type){
  plot <- ggplot(data=plotData,
                 aes(x=opp, y=error, group=label, shape=label, linetype=label, color=label, fill=label)) +
    geom_smooth(data=plotData,linetype=0) +
    stat_summary(fun.y="mean", geom="line") +
    stat_summary(fun.y="mean", geom="point") +
    coord_cartesian(ylim=c(0, 1)) +
    labs(title=title) +
    ylab("Error Rate") +
    xlab("Opportunities") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size=rel(label.scale)),
      axis.text = element_text(size=rel(label.scale)),
      axis.title = element_text(size=rel(label.scale)),
      legend.position = legend.position
    )
  }
  else if (substring('smooth', 1, nchar(line.type)) == line.type) {
    plot <- ggplot(data=plotData,
                   aes(x=opp, y=error, group=label, linetype=label, color=label, fill=label)) +
      geom_smooth(data=plotData) +
      coord_cartesian(ylim=c(0, 1)) +
      labs(title=title) +
      ylab("Error Rate") +
      xlab("Opportunities") +
      ggtitle(title) +
      theme(
        plot.title = element_text(size=rel(label.scale)),
        axis.text = element_text(size=rel(label.scale)),
        axis.title = element_text(size=rel(label.scale)),
        legend.position = legend.position
      )
  }
  else {
    warning(paste0('Unrecognized line.type "',line.type,'"'))
    plot <- NULL
  }

  return(plot)
}


#' Read in a DataShop Student-Step rollup file
#'
#' @param file.name The name of the file to read in.
ds.read <- function(file.name) {
  return(read.csv(file = file.name,sep="\t",check.names=F))
}


#' Save a ds.plot in a commonly useful png format.
#'
#' @param plot   The plot data
#' @param name   The desired filename
#' @param width  The width in inches (defaults to 3, which is recommended width for AcM/EDM format)
#' @param height The height in inches (which I default to 2.5)
#' @param dpi    The dpi to output at (defaults to 300)
ds.save <- function(plot, name, width=3.3, height=2.08, dpi=300) {
  ggsave(name,plot=plot,width=width,height=height,dpi=dpi,units="in")
}



#' Return a list of the available KC models in a Student-Step file.
#'
#' @param stu.step A Student-Step export from datashop
ds.models <- function(stu.step) {
  x <- names(stu.step)
  m <- regexpr("KC (.*)",x,perl=TRUE)
  mods <- regmatches(x,m)
  return(unlist(lapply(mods, function(s) (substr(s,5,nchar(s)-1)))))
}

#' Return a list of the kcs within a partiuclar model.
#' 
#' @param stu.step A DataShop Student-Step export data.frame
#' @param model    The name of a KC model within the dataset
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