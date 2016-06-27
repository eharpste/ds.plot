library(ggplot2)

#' A Smooth Plot DataShop Learning Curves
#'
#' Creates a loess smoothed ggplot from a student step roll up output from Datashop.
#'
#' @param stu.step A data.frame of a direct load of a student-step rollup file.
#' @param model The name of a KC model to use as it appears in DataShop
#' @param kc The name of a particular Kc to get individual kc curves or its number in the list of kcs
#' @param title A title for the plot
#' @parm axis.scale A scaling factor for the y axis of the plot.
#' @keywords ds.plot
#' @export
#' @examples

ds.plot <- function(stu.step, model, kc="all", title="",axis.scale=1){

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

  if(title == "") {
    if(kc.name == "all") {
      title = model
    }
    else {
      title = kc.name
    }
  }

  plot <- ggplot() +
    geom_smooth(data=plotData, aes(x=opp, y=error, group=label, color=label, fill=label)) +
    coord_cartesian(ylim=c(0, 1)) +
    labs(title=title) +
    ylab("Error Rate") +
    xlab("Opportunities") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size=rel(axis.scale)),
      axis.text = element_text(size=rel(axis.scale)),
      axis.title = element_text(size=rel(axis.scale)),
      legend.position = "none"
    )

  return(plot)
}
