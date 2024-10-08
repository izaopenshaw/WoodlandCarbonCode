% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{error_product}
\alias{error_product}
\title{Analytical error progression for product}
\usage{
error_product(a, sig_a, b, sig_b, c = NA, sig_c = NA)
}
\arguments{
\item{a}{first variable in product}

\item{sig_a}{sigma for a}

\item{b}{second variable in product}

\item{sig_b}{sigma for b}

\item{c}{(optional) third variable in product}

\item{sig_c}{(optional) sigma for c}
}
\value{
error for x = (abs(a * b) * sqrt((sig_a / a)^2 + (sig_b / b)^2))^2
or (abs(a * b * c) * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2))^2
}
\description{
Calculates the error for x when x = a * b or x = a * b * c
}
\references{
Taylor, J. R. (1997). An Introduction to Error Analysis: The Study of Uncertainties in Physical Measurements (2nd ed.). University Science Books.
OR Bevington, P. R., & Robinson, D. K. (2003). Data Reduction and Error Analysis for the Physical Sciences (3rd ed.). McGraw-Hill.
}
\author{
Isabel Openshaw. I.Openshaw@kew.org
}
