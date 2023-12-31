% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suppress.R
\name{suppress}
\alias{suppress}
\title{Suppress data according to APDE standards or custom requests & adding caution flag for high RSE}
\usage{
suppress(
  sup_data = NULL,
  suppress_range = c(0, 9),
  secondary = FALSE,
  secondary_ids = c("tab", "indicator_key", "cat1", "cat2_group", "year"),
  secondary_exclude,
  flag_only = FALSE
)
}
\arguments{
\item{sup_data}{a data.table or data.frame. Must contain the data to be suppressed with standard metric names,
i.e., mean, median, sum, rate, lower, upper, se, rse, numerator, denominator, proportion}

\item{suppress_range}{integer vector of length 2. They specify the minimum and maximum range for suppression.}

\item{secondary}{logical (T, TRUE, F, or FALSE) indicating whether secondary suppression should be run}

\item{secondary_ids}{character vector of column names which are used to define groups for secondary suppression.
Note, this should not include the most granular level. For example, if you wanted secondary suppression for race/ethnicity
where category == race/ethnicity and group == AIAN, Asian, Black, etc., you should have
secondary_ids = c("geography", "category") rather than secondary_ids = c("geography", "category", "group")}

\item{secondary_exclude}{an expression identifying the rows to be filtered / excluded from secondary suppression because
the categories are not mutually exclusive (e.g., race3)}

\item{flag_only}{logical (T, TRUE, F, or FALSE) indicating whether data to be suppressed should be flagged without setting estimates to NA}
}
\value{
a data.table with suppression applied to CHI standard columns.
}
\description{
Default suppression is according to APDE/DOH standards (https://www.doh.wa.gov/Portals/1/Documents/1500/SmallNumbers.pdf)
Each dataset may have it's own more stringent standards. When the reference sheet of all suppression guidelines is
made available, this code should be updated to use that resource.

This function expects data that has already been formatted for CHI
}
\examples{

  set.seed(98104)
  dt <- data.table::data.table(
    chi_year = rep(2018, 100),
    mean = rnorm(100, .25, .05),
    numerator = round(rnorm(100, 20, 9), 0)
  )
  table(dt$numerator) # before
  newdt <- suppress(dt, suppress_range = c(0, 20), secondary = FALSE) # apply suppression
  table(newdt$numerator) # after

}
\keyword{suppression}
