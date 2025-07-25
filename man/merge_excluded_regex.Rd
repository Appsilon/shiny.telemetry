% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{merge_excluded_regex}
\alias{merge_excluded_regex}
\title{Merge a list of regular expressions into a single one}
\usage{
merge_excluded_regex(regex_l, trim_whitespace = TRUE)
}
\arguments{
\item{regex_l}{list of regular expressions to be merged.}

\item{trim_whitespace}{boolean indicating if whitespace should be trim
from the start and end of the regex list.}
}
\value{
Single regular expression string.
}
\description{
Merge a list of regular expressions into a single one
}
\keyword{internal}
