\name{app_server}
\alias{app_server}
\title{PopPK FitCheckR Server Function}
\usage{
app_server(input, output, session)
}
\arguments{
  \item{input, output, session}{Shiny server arguments.}
}
\value{
Called for its side effects.
}
\description{
Server logic for the application: file upload and column mapping, data
filtering, GOF plots, the interactive GOF plot, and correlation plots.
}
\seealso{
\code{\link{app_ui}}, \code{\link{run_app}}
}
