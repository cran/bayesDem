\name{bayesDem.go}
\Rdversion{1.1}
\alias{bayesDem.go}
\title{
Start Graphical User Interface for bayesTFR and bayesLife
}
\description{
The function launches Graphical User Interface for the packages \pkg{\link[bayesTFR:bayesTFR-package]{bayesTFR}} and \pkg{\link[bayesLife:bayesLife-package]{bayesLife}}.
}
\usage{
bayesDem.go(wpp.year.tfr = tfr.wpp.year.default, 
    wpp.year.e0 = e0.wpp.year.default,
    wpp.year.pop = pop.wpp.year.default)
}
\arguments{
  \item{wpp.year.tfr}{WPP year used in the bayesTFR package. Default is 2010. Note that the GUI will work only with values for which observed data are available in the bayesTFR package. Currently it is 2010 and 2008, see \pkg{\link[bayesTFR]{UN2010}}.}
  \item{wpp.year.e0}{WPP year used in the bayesLife package. Default is 2010. Currently years 2010 and 2008 are available, see \pkg{\link[bayesLife]{UN2010e0}}.}
   \item{wpp.year.pop}{WPP year used in the bayesPop package. Default is 2010. Currently years 2010 and 2008 are available, see \code{\link[bayesPop:bayesPop-package]{bayesPop}}.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Hana Sevcikova
}
\note{
For more information on World Population Prospects (WPP) data, see \url{http://esa.un.org/unpd/wpp}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ programming }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
