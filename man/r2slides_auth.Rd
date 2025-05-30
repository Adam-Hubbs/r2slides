% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authentication.R
\name{r2slides_auth}
\alias{r2slides_auth}
\title{Create OAuth authentication token}
\usage{
r2slides_auth(
  email = gargle::gargle_oauth_email(),
  scopes = c("https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/presentations",
    "https://www.googleapis.com/auth/drive"),
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default()
)
}
\arguments{
\item{email}{Optional. A string giving an email address that can be used to identify the user.}

\item{scopes}{Optional. A character vector of requested OAuth scopes.}

\item{cache}{Optional. Specifies the OAuth token cache.}

\item{use_oob}{Optional. Whether to use out-of-band authentication.}
}
\value{
Nothing directly. Updates the .auth token. Will error if unable to retrieve a valid
OAuth2.0 token.
}
\description{
Create OAuth authentication token
}
\keyword{internal}
