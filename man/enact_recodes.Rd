% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enact_recodes.R
\name{enact_recodes}
\alias{enact_recodes}
\title{Apply a set of recode instructions to a dataset}
\usage{
enact_recodes(data, ..., ignore_case = TRUE, copy = TRUE)
}
\arguments{
\item{data}{data.frame (or something that inherits from a data frame)}

\item{...}{objects coercible to a list of \code{recode_instructions}}

\item{ignore_case}{logical. should the case of names(data) be ignored?}

\item{copy}{logical. If false and data is a data.table object, the function omits a copy step and will alter the underlying data.table by reference}
}
\description{
Apply a set of recode instructions to a dataset
}
