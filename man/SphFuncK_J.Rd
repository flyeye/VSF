% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vsf.R
\name{SphFuncK_J}
\alias{SphFuncK_J}
\title{Calculation of the spherical harmonic value with combined index j in the point (l, b)}
\usage{
SphFuncK_J(j, l, b, where = WHOLE_SPHERE)
}
\arguments{
\item{j}{- combined index, integer}

\item{l}{- longitude, real}

\item{b}{- latitude, real}

\item{where}{- flag, where the value must be calculated, 1 - whole sphere, 0 - hemisphere}
}
\value{
spherical harmonic value with indexes n, k and p in the point(l, b)
}
\description{
Calculation of the spherical harmonic value with combined index j in the point (l, b)
}
