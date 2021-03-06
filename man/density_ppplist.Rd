% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intensity.R
\name{density_ppplist}
\alias{density_ppplist}
\title{Intensity Estimate for Ppp List}
\usage{
density_ppplist(xl, sigmas)
}
\arguments{
\item{xl}{list of ppp-objects}

\item{sigmas}{vector of bandwidths, as in density.ppp (sd of kernel)}

\item{...}{further arguments passed on to density.ppp}
}
\description{
Intensity Estimate for Ppp List
}
\details{
Wrapper for computing intensity estimates for a list of point patterns. Note that this is like
\code{density.ppp} in spatstat except each pattern is assigned own bandwidth.
}
