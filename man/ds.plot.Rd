% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{ds.plot}
\alias{ds.plot}
\title{Plot Learning Curves from DataShop Student-Step export data}
\usage{
ds.plot(stu.step, model, kc = "all", line.type = "smooth", title = NULL,
  legend.position = "none", label.scale = 1)
}
\arguments{
\item{stu.step}{A data.frame of a direct load of a student-step rollup file.}

\item{model}{The name of a KC model to use as it appears in DataShop}

\item{kc}{The name of a particular Kc to get individual kc curves or its number in the list of kcs}

\item{line.type}{Controls whether to render a "smooth" loess line or "discrete" true averages line.}

\item{title}{A title for the plot}

\item{legend.position}{defines the position of a legend, defaults to "none" because I usually explain the legend in a figure capion.}

\item{label.scale}{A scaling factor for the axis label and title text.}
}
\description{
The main drawback to DataShop's default plotting representation is that it does not properly convey that
N is dropping off in a mastery learning paradigm. This function renders a plot with a loess smoothed
confidence channel around the real data and the AFM estimates to convey this trend. The curves themsevles
can also be plotted loess smoothed (default) for with discrete averages like standard DataShop
}
\keyword{ds.plot}

