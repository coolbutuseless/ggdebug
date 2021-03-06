% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_geom.R
\name{get_geom}
\alias{get_geom}
\title{Get the name of the 'geom' which is using this stat.}
\usage{
get_geom()
}
\value{
name of geom, otherwise NA_character_
}
\description{
This needs to be called from within a Stat method.  It interrogates the
frame stack to figure out the layer which called the Stat, and then
gets the Geom from that layer.
}
\details{
Written with the help of Brodie Gaslam
}
\examples{
\dontrun{
  setup_params = function(data, params) {
   message("StatOathBreaker being used with: ", get_geom())
   params
 }
}
}
