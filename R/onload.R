#Create null .auth so .onLoad can work properly
.auth <- NULL

# Silence CRAN check over missing global binding (because we make it when the user registers/creates a presentation)
if(getRversion() >= "2.15.1") utils::globalVariables(c("google_presentation"))


#Set up when package is created
.onLoad <- function(libname, pkgname) {

  utils::assignInMyNamespace(
    ".auth",
    gargle::init_AuthState(package = "r2slides", auth_active = TRUE)
  )

}


# TODO Need to add gs discovery document into mthds (and combine slides and gs into one top level directory?)
# Use gargle internal functions to extract and clean discovery document


### This is code to create and save the discovery package in a internal only way in R --
# For Development only. Read in low level function helpers for working with the discovery document
# source(system.file("discovery-doc-ingest/ingest-functions.R", package = "gargle"))
#
# slidesDiscDoc <- read_discovery_document('/Users/adamhubbs/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Active Projects_AH/Google Slides Automation/Slides Disc Doc.json')
#
# mthds <- get_raw_methods(slidesDiscDoc)
#
# usethis::use_data(mthds, internal = TRUE)
