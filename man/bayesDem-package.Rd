\name{bayesDem-package}
\Rdversion{1.1}
\alias{bayesDem-package}
\alias{bayesDem}
\docType{package}
\title{
Graphical User Interface for bayesTFR and bayesLife}
\description{
Provides Graphical user interface for the packages \pkg{bayesTFR} and \pkg{bayesLife}.}
\details{
\tabular{ll}{
Package: \tab bayesDem\cr
Version: \tab 1.3-2\cr
Date: \tab 2011-10-20\cr
Depends: \tab R (>= 2.9.0), cairoDevice, gWidgets, gWidgetsRGtk2, bayesTFR, bayesLife\cr
License: \tab  GPL (>= 2)\cr
URL: \tab \url{http://bayespop.csss.washington.edu}\cr
}

The main function of the package is \code{\link{bayesDem.go}} which launches the GUI. It allows to run MCMCs and make probabilistic projections of total fertility rate (TFR) and life expectancy (e0) for all countries of the world, using Bayesian hierarchical models and the United Nations demographic time series. 

The GUI has two main tabs, one for estimating and predicting TFR, the other for estimating and predicting e0. Each of these two sections is organized into four tabs corresponding to the four main tasks: Running MCMCs, Continuing MCMCs, Making predictions, and Exploring results. Each of those tabs is again separated into tabs corresponding to usually one \pkg{bayesTFR} or \pkg{bayesLife} function. Each tab contains a Help button that shows the help file of the corresponding function, a \sQuote{Generate Script} button that provides user with the corresponding \pkg{bayesTFR}/\pkg{bayesLife} command with argument values filled in from the GUI, and a button processing the function.
}
\author{
Hana Sevcikova <hanas@u.washington.edu>

Maintainer: Hana Sevcikova <hanas@uw.edu>
}
\references{
Hana Sevcikova, Leontine Alkema, Adrian E. Raftery (2011). bayesTFR: An
  R Package for Probabilistic Projections of the Total Fertility Rate.
  Journal of Statistical Software, 43(1), 1-29.
  \url{http://www.jstatsoft.org/v43/i01/}.
}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation ~~
%~~ directory ~~
\keyword{ package }
\seealso{
 \code{\link[bayesTFR:bayesTFR-package]{bayesTFR}}, \code{\link[bayesLife:bayesLife-package]{bayesLife}}
}
\examples{
\dontrun{
bayesDem.go()
}
}
