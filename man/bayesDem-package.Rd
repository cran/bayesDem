\name{bayesDem-package}
\alias{bayesDem-package}
\alias{bayesDem}
\docType{package}
\title{
Graphical User Interface for bayesTFR, bayesLife and bayesPop}
\description{
Provides Graphical user interface for the packages \pkg{bayesTFR}, \pkg{bayesLife} and \pkg{bayesPop}.}
\details{
\tabular{ll}{
Package: \tab bayesDem\cr
Version: \tab 2.5-1\cr
Date: \tab 2016-11-21\cr
License: \tab  GPL (>= 2)\cr
URL: \tab \url{https://bayespop.csss.washington.edu}\cr
}

The main function of the package is \code{\link{bayesDem.go}} which launches the GUI. It allows to run MCMCs and make probabilistic projections of total fertility rate (TFR) and life expectancy (e0) for all countries of the world, using Bayesian hierarchical models and the United Nations demographic time series, as well as probabilistic projections of total population.

The GUI has three main tabs, one for estimating and predicting TFR, one for estimating and predicting e0, and one for projecting total population. 

The first two sections are organized into four tabs corresponding to the four main tasks: Running MCMCs, Continuing MCMCs, Making predictions, and Exploring results. Each of those tabs is again separated into tabs corresponding to usually one \pkg{bayesTFR} or \pkg{bayesLife} function. Each tab contains a Help button that shows the help file of the corresponding function, a \sQuote{Generate Script} button that provides user with the corresponding \pkg{bayesTFR}/\pkg{bayesLife} command with argument values filled in from the GUI, and a button processing the function. 

The third main section (for projecting total population) is separated into two tabs: One for generating predictions using given inputs, the other for exploring results. The user can generate population trajectories for given country, age and sex. In addition, probabilistic population pyramids can be also created from the GUI.   
}
\author{
Hana Sevcikova <hanas@u.washington.edu>

Maintainer: Hana Sevcikova <hanas@uw.edu>
}
\references{
A. E. Raftery,  N. Li, H. Sevcikova , P. Gerland, G. K. Heilig (2012). Bayesian probabilistic population projections for all countries. Proceedings of the National Academy of Sciences 109:13915-13921.

Hana Sevcikova, Leontine Alkema, Adrian E. Raftery (2011). bayesTFR: An
  R Package for Probabilistic Projections of the Total Fertility Rate.
  Journal of Statistical Software, 43(1), 1-29.
  \url{http://www.jstatsoft.org/v43/i01/}.
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation ~~
%~~ directory ~~
\keyword{ package }
\seealso{
 \code{\link[bayesTFR:bayesTFR-package]{bayesTFR}}, \code{\link[bayesLife:bayesLife-package]{bayesLife}}, \code{\link[bayesPop:bayesPop-package]{bayesPop}}
}
\examples{
\dontrun{
bayesDem.go()
}
}
