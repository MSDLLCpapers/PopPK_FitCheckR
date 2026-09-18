\name{run_app}
\alias{run_app}
\title{Launch the PopPK FitCheckR Application}
\usage{
run_app(launch.browser = TRUE, ...)
}
\arguments{
  \item{launch.browser}{Whether to launch the app in a web browser.}
  \item{...}{Additional arguments passed to \code{shiny::runApp()}.}
}
\value{
The Shiny app object, invisibly.
}
\description{
Launches the packaged Shiny application for NONMEM goodness-of-fit diagnostics
and correlation plots.
}
