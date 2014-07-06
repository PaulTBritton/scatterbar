\name{plotlist}
\alias{plotlist}
\alias{alist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List of object names
%%  ~~function to do ... ~~
}
\description{
List of object names
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotlist(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
	objects
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	see alist()
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul Thomas Britton
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{scatterbar}}, \code{\link{alist}}
}
\examples{
x <- c(.1,.2,.3)
y <- c(.01,.02,.03)
plist <- plotlist(x,"why"=y)
print(plist)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
#function (...) 
#as.list(sys.call())[-1L]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ alist }
\keyword{ scatterbar }% __ONLY ONE__ keyword per line
