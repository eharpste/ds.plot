#' Plot Learning Curves from DataShop Student-Step export data
#'
#' The main drawback to DataShop's default plotting representation is that it does not properly convey that
#' N is dropping off in a mastery learning paradigm. This function renders a plot with a loess smoothed
#' confidence channel around the real data and the AFM estimates to convey this trend. The curves themsevles
#' can also be plotted loess smoothed (default) for with discrete averages like standard DataShop
#'
#' @param stu.step        A data.frame of a direct load of a student-step rollup file.
#' @param model           The name of a KC model to use as it appears in DataShop
#' @param kc              The name of a particular Kc to get individual kc curves or its number in the list of kcs
#' @param title           A title for the plot
#' @param line.type       Controls whether to render a "smooth" loess line or "discrete" true averages line.
#' @param label.scale     A scaling factor for the axis label and title text.
#' @param legend.position defines the position of a legend, defaults to "none" because I usually explain the legend in a figure capion.
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
    plot <- ggplot2::ggplot(data=plotData,
              ggplot2::aes(x=opp, y=error, group=label, shape=label, linetype=label, color=label, fill=label)) +
              ggplot2::geom_smooth(data=plotData,linetype=0) +
              ggplot2::stat_summary(fun.y="mean", geom="line") +
              ggplot2::stat_summary(fun.y="mean", geom="point") +
              ggplot2::coord_cartesian(ylim=c(0, 1)) +
              ggplot2::labs(title=title) +
              ggplot2::ylab("Error Rate") +
              ggplot2::xlab("Opportunities") +
              ggplot2::ggtitle(title) +
              ggplot2::theme(
                        plot.title = ggplot2::element_text(size=ggplot2::rel(label.scale)),
                        axis.text = ggplot2::element_text(size=ggplot2::rel(label.scale)),
                        axis.title = ggplot2::element_text(size=ggplot2::rel(label.scale)),
                        legend.position = legend.position
    )
  }
  else if (substring('smooth', 1, nchar(line.type)) == line.type) {
    plot <- ggplot2::ggplot(data=plotData,
                            ggplot2::aes(x=opp, y=error, group=label, linetype=label, color=label, fill=label)) +
      ggplot2::geom_smooth(data=plotData) +
      ggplot2::coord_cartesian(ylim=c(0, 1)) +
      ggplot2::labs(title=title) +
      ggplot2::ylab("Error Rate") +
      ggplot2::xlab("Opportunities") +
      ggplot2::ggtitle(title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size=ggplot2::rel(label.scale)),
        axis.text = ggplot2::element_text(size=ggplot2::rel(label.scale)),
        axis.title = ggplot2::element_text(size=ggplot2::rel(label.scale)),
        legend.position = legend.position
      )
  }
  else {
    warning(paste0('Unrecognized line.type "',line.type,'"'))
    plot <- NULL
  }

  return(plot)
}