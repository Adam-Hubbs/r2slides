#Create null .auth so .onLoad can work properly
.auth <- NULL

# Silence CRAN check over missing global binding (because we make it when the user registers/creates a presentation)
if(getRversion() >= "2.15.1") utils::globalVariables(c("google_presentation"))

if(getRversion() < "4.4.0") {
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
}


#Set up when package is created
.onLoad <- function(libname, pkgname) {

  utils::assignInMyNamespace(
    ".auth",
    gargle::init_AuthState(package = "r2slides", auth_active = TRUE)
  )

  S7::methods_register()

}


# TODO Need to add gs discovery document into mthds (and combine slides and gs into one top level directory?)
# Use gargle internal functions to extract and clean discovery document


### This is code to create and save the discovery package in a internal only way in R --
# For Development only. Read in low level function helpers for working with the discovery document
# source(system.file("discovery-doc-ingest/ingest-functions.R", package = "gargle"))

# slidesDiscDoc <- read_discovery_document('/Users/adamhubbs/Y2 Analytics Dropbox/Y2 Analytics Team Folder/Active Projects_AH/Google Slides Automation/Slides Disc Doc.json')

# sheetsDiscDoc <- read_discovery_document(
#   '/Users/adamhubbs/Y2 Analytics Dropbox/Adam Hubbs/Development/Google Slides Automation/Sheets Disc Doc.json'
# )

# client_json_path <- '/Users/adamhubbs/Y2 Analytics Dropbox/Adam Hubbs/y2_client.json'
# client_json <- readLines(client_json_path)

# #client_json <-jsonlite::fromJSON(client_json)
# .json_key <- gargle::secret_make_key()
# # secret_json <- gargle::secret_encrypt_json(client_json, key = .json_key)
# secret_write_rds(client_json, path = "client_json.rds", key = .json_key)

# mthds_slides <- get_raw_methods(slidesDiscDoc)
# mthds_sheets <- get_raw_methods(sheetsDiscDoc)


# usethis::use_data(mthds_slides, mthds_sheets, .json_key, internal = TRUE, overwrite = TRUE)



