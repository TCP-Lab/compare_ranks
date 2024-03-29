% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{score_comparisons}
\alias{score_comparisons}
\title{Compute scores for two-way comparisons}
\usage{
score_comparisons(comparisons)
}
\arguments{
\item{comparisons}{A data.frame with at least these columns:
- \code{steps}: the window size of the calculated rank;
- \code{step_fraction}: \code{size} but divided by the length of the vectors;
Each additional column is treated as a comparison}
}
\value{
A list with one slot per additional column in the input with the value
of the computed metric for that comparison.
}
\description{
It can be useful to summarize to a single score the comparisons.
This can be done by computing the area under the curve, akin to what is done
with receiver operating characteristics curves.
}
\author{
Hedmad
}
