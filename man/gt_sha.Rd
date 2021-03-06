% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gt_sha}
\alias{gt_sha}
\title{gt package git commit SHA}
\format{An object of class \code{character} of length 1.}
\usage{
gt_sha
}
\description{
This is the git commit SHA for the gt package associated with the current
release of the gtsummary package. Because the gt package is currently under
development, breaking changes are more likely to be introduced. This git
commit SHA references a version of the gt package compatible with the
current release of gtsummary. Use the SHA to install the appropriate version
of the gt package tested for compatibility with gtsummary.
}
\examples{
gtsummary::gt_sha

# install gt version compatible with gtsummary release
# remotes::install_github("rstudio/gt", ref = gtsummary::gt_sha)
}
\keyword{datasets}
