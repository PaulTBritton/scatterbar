\name{scatterbar}
\alias{scatterbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot scatterbars from a list of number vectors
%%  ~~function to do ... ~~
}
\description{
plot scatterbars from a list of number vectors
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scatterbar(file,envir=parent.frame(),filter=".*",
		plist=mylst(ls(envir,pattern=filter),envir),
		X=setnames(plist,envir),logaxis="",rmarg=8,
		xnotation=sciNotation,prec=2,stats=c(2,2,2,2),maintitle=NULL,
		units="Probability",sbox=FALSE,stext=FALSE,
		tsize=gettsize(getT(length(X))),tpos=gettpos(getT(length(X))),
		xmarks=calcxmarks(min(unlist(X)),max(unlist(X)),logaxis),
		range=calcrange(min(unlist(X)),max(unlist(X)),logaxis),
		lpos=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
	Optional Name of tiff file for plot output
%%     ~~Describe \code{file} here~~
}
  \item{envir}{
	Environment containing the number vectors to be plotted
%%     ~~Describe \code{envir} here~~
}
  \item{filter}{
	Wildcard pattern to match which number vectors to plot
%%     ~~Describe \code{filter} here~~
}
  \item{plist}{
	Explicit plotlist of object names and (optional) descriptions of objects to plot
%%     ~~Describe \code{lst} here~~
}
  \item{X}{
	Explicit named list of objects to plot
%%     ~~Describe \code{lst} here~~
}
  \item{logaxis}{
	"x" is log scale on x-axis and "" is linear
%%     ~~Describe \code{logaxis} here~~
}
  \item{rmarg}{
	right margin
%%     ~~Describe \code{rmarg} here~~
}
  \item{xnotation}{
	numbe format on x-axis
%%     ~~Describe \code{xnotation} here~~
}
  \item{prec}{
	numbe of significant digits
%%     ~~Describe \code{prec} here~~
}
  \item{stats}{
	a vector of positive integers describing which sample statistics to display on the scatterbars
	
%%     ~~Describe \code{stats} here~~
}
  \item{maintitle}{
	main title of plot
%%     ~~Describe \code{maintitle} here~~
}
  \item{units}{
	label for x-axis units. e.g. probability, length, etc.
%%     ~~Describe \code{units} here~~
}
  \item{sbox}{
	5th to 95th box
%%     ~~Describe \code{sbox} here~~
}
  \item{stext}{
	print sample stats text on sbox
%%     ~~Describe \code{stext} here~~
}
  \item{tsize}{
	overide automatic text size for stext
%%     ~~Describe \code{tsize} here~~
}
  \item{tpos}{
	overide automatic text positioning for stext
%%     ~~Describe \code{tpos} here~~
}
  \item{xmarks}{
	location of x-axis tick marks
%%     ~~Describe \code{xmarks} here~~
}
  \item{range}{
	range of x-axis
%%     ~~Describe \code{range} here~~
}
  \item{lpos}{
	position of optional legend
%%     ~~Describe \code{lpos} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
tiff file output
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
\code{\link{plotlist}},\code{\link{alist}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(numform)
x <- c(.1,.2,.3)
y <- c(.2,.02,.03)
scatterbar(file="xyplot.tiff",plist=plotlist(x,y))
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plotlist }
