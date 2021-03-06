% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bloom.R
\name{bloom}
\alias{bloom}
\title{Compute aggregate functions on subsets of a dataset}
\usage{
bloom(datatable, cols, retain_all = TRUE, fn)
}
\arguments{
\item{datatable}{a data.table object}

\item{cols}{columns to group by and retain after aggregation}

\item{retain_all}{hold back an `All` option for each col supplied in cols}

\item{fn}{a function taking a datatable that returns}
}
\description{
Creates subsets of a dataset and then applies aggregate functions over it, while retaining knowledge of which filtering conditions were applied.
}
