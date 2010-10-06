\name{bayesDem-package}
\Rdversion{1.1}
\alias{bayesDem-package}
\alias{bayesDem}
\docType{package}
\title{
Graphical User Interface for bayesTFR}
\description{
Provides Graphical user interface for the package \pkg{bayesTFR}.}
\details{
\tabular{ll}{
Package: \tab bayesDem\cr
Version: \tab 1.0-0\cr
Date: \tab 2010-10-06\cr
Depends: \tab R (>= 2.9.0), cairoDevice, gWidgets, gWidgetsRGtk2, bayesTFR\cr
License: \tab  GPL (>= 2)\cr
URL: \tab \url{http://bayespop.csss.washington.edu}\cr
}

The main function of the package is \code{\link{bayesDem.go}} which launches the GUI. It allows to run MCMCs and make probabilistic projections of total fertility rate (TFR) for all countries of the world, using a Bayesian hierarchical model and the United Nations demographic time series.

The GUI is organized into four tabs corresponding to the four main tasks: Running MCMCs, Continuing MCMCs, Making predictions, and Exploring results. Each of those tabs is again separated into tabs corresponding to usually one \pkg{bayesTFR} function. Each tab contains a Help button that shows the help file of the corresponding function, a \sQuote{Generate Script} button that provides user with the corresponding \pkg{bayesTFR} command with argument values filled in from the GUI, and a button processing the function.
}
\author{
Hana Sevcikova <hanas@u.washington.edu>

Maintainer: Hana Sevcikova <hanas@uw.edu>
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation ~~
%~~ directory ~~
\keyword{ package }
\seealso{
 \code{\link[bayesTFR:bayesTFR-package]{bayesTFR}}
}
\examples{
\dontrun{
bayesDem.go()
}
}
