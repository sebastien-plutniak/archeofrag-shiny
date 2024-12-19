\name{archeofrag.gui}
\alias{archeofrag.gui}

\title{archeofrag.gui}
\description{Launch the archeofrag.gui application}

\usage{
archeofrag.gui()
}
\details{

This function launches the `archeofrag.gui` application.

Note that the  optional determination of graph planarity in  the \code{\link[archeofrag]{archeofrag}} package requires the  \code{RBGL} package.
}
\value{
    Launch the `archeofrag.gui` R Shiny application.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
  \code{\link[archeofrag]{archeofrag}}
	\code{\link[archeofrag]{frag.simul.process}}
	\code{\link[archeofrag]{frag.get.parameters}}
}

\examples{
    \dontrun{
    # launching the app:
    archeofrag.gui()
    }
}
