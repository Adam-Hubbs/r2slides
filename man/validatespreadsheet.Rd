% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spreadsheet_helpers.R
\name{validatespreadsheet}
\alias{validatespreadsheet}
\title{Validate spreadsheet environment}
\usage{
validatespreadsheet(spreadsheet)
}
\arguments{
\item{spreadsheet}{A single string naming a spreadsheet environment.}
}
\value{
Nothing. The function will error if the specified environment doesn't
exist in `.GlobalEnv` or if it's missing any of the required objects
(`spreadsheet_id`, `sheet_ids`).
}
\description{
Validate spreadsheet environment
}
\keyword{internal}
