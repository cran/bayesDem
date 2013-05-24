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
bayesDem.go(wpp.year.tfr = wpp.year.default, 
    wpp.year.e0 = wpp.year.tfr, wpp.year.pop = wpp.year.tfr)
}
\arguments{
  \item{wpp.year.tfr}{WPP year used for simulating TFR. Default is 2010. Packages called \pkg{wpp}\eqn{x} where \eqn{x} is the \code{wpp.year} are used. Currently, 2010 and 2008 are available.}
  \item{wpp.year.e0}{WPP year used for simulating life expectancy.}
   \item{wpp.year.pop}{WPP year used for predicting population.}
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
