% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{plot_continuous_congruency}
\alias{plot_continuous_congruency}
\title{Plot the results of one or more continuous congruency comparisons}
\usage{
plot_continuous_congruency(comparisons, legend_scale = 10)
}
\arguments{
\item{comparisons}{A dataframe with at least these columns:
- \code{steps}: the window size of the calculated rank;
- \code{step_fraction}: \code{size} but divided by the length of the vectors;
Each additional column is treated as a comparison, and will
be plotted with a label equal to the column name.}

\item{legend_scale}{The scale of the legend. With many comparisons, it might
be a good idea to make the legend smaller by reducing
this value. Defaults to 10.}
}
\description{
The plot can show how similar each comparison is by drawing a line with the
value of the congruency metric in function of the width size.
}
\author{
Hedmad
}
