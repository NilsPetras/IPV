% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_excel.R
\name{input_excel_factor}
\alias{input_excel_factor}
\title{Input Excel Factor}
\usage{
input_excel_factor(file, raw = FALSE)
}
\arguments{
\item{file}{character; filename of the excel file}

\item{raw}{logical; should raw factor loading estimates be returned instead?;
defaults to FALSE}
}
\value{
list containing formatted data including center distances for
  \code{\link{item_chart}}, \code{\link{facet_chart}} or factor loadings if
  \code{raw = TRUE}.
}
\description{
Reads factor loadings and latent correlations from an excel file.
}
\details{
Helper function of \code{\link{input_excel}}.
}
\seealso{
\code{\link{input_excel}}
}
