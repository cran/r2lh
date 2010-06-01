\name{r2hMainFile}
\alias{r2hMainFile}

\title{~ Generation of HTML main document ~}
\description{
  This function generates an HTML main document designed to include
  links to some files built by \code{r2hu} or \code{r2hb}.
}
\usage{
r2hMainFile(fileOut = "main.html",text="<OBJECT data='univ.html' type='text/html'></OBJECT>",sweave=FALSE)
}

\arguments{
  \item{fileOut}{\code{characters}; name of the main document.}
  \item{text}{text to be included in the HTML document, between the
    '<BODY>' and '</BODY>' .
  }
  \item{sweave}{\code{logical}; if \code{TRUE}, the main document is
    designed to be compiled with \code{Sweave}.}
}

\details{
  \code{r2hMainFile} generates an HTML master file ready to include links to some
  files built by \code{r2hu} or \code{r2hb}.
}
\value{
   A HTML file ("main.html" by default).
}

\section{Author}{
  Christophe Genolini \cr
  \email{christophe.genolini@free.fr}\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health \cr
  INSERM U669 / Maison de Solenn / Paris
  \cr
  Bernard Desgraupes \cr
  \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre
}


\references{HTML web site \url{http://www.w3.org/TR/html/}\cr
Data are available on line: \url{http://christophe.genolini.free.fr/EPO/EPO2007-Fraude.php}}



\seealso{
  \code{\link{r2hu}},
  \code{\link{r2hb}},
  \code{\link{r2lh-package}},
  \code{\link{examCheating}},
  \code{\link{Sweave}}
}

\examples{
 # # # # # # # # # # # # # # # # # # #
#   R to HTML, Main file generation   #
 #             Examples              #
  #           r2hMainFile           #
   # # # # # # # # # # # # # # # # #


### Creates a file named "main.html" that includes "univ.html"
if(!file.exists("tmp")){dir.create("tmp")};setwd("tmp")
r2hMainFile()
setwd("../..")
}


\keyword{print}        % Printing
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
