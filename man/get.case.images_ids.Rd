% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcga.R
\name{get.case.images_ids}
\alias{get.case.images_ids}
\title{get.case.images_ids}
\usage{
get.case.images_ids(case.id)
}
\arguments{
\item{case.id}{an uuid of a TCGA case}
}
\value{
the list of images that are related to the TCGA case specified
}
\description{
Return the list of the TCGA image ids (both of Diagnostic Slide and Tissue Slide)
related to a given case.id.
}
\examples{
# Where 942c0088-c9a0-428c-a879-e16f8c5bfdb8 is the case.id of
# the case with case.symbol TCGA-CJ-4642
get.case.images_ids("942c0088-c9a0-428c-a879-e16f8c5bfdb8")

}
