% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_debug.R
\name{enable_caching_in_stat_function}
\alias{enable_caching_in_stat_function}
\title{Add debugging to a particular function within a Stat}
\usage{
enable_caching_in_stat_function(DebugStat, stat_original_name, method,
  reset_cache = FALSE, verbose = FALSE, head_n)
}
\arguments{
\item{DebugStat}{the actual Stat}

\item{stat_original_name}{full name of original stat e.g. 'StatIdentity'. Used
for debugging output}

\item{method}{the method name within this stat to debug}

\item{reset_cache}{If TRUE then the cache of captured data is
reset when this Stat$method() is invoked. Default: FALSE.}

\item{verbose}{Print data information as the methods execute. Default: FALSE}

\item{head_n}{Argument to \code{head()} when printing data.frames in verbose mode}
}
\description{
Add debugging to a particular function within a Stat
}
