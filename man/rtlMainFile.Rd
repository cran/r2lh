\name{rtlMainFile}
\alias{rtlMainFile}

\title{~ Generation of LaTeX main document ~}
\description{
  This function generates a LaTeX main document designed to include some
  files built by \code{rtlu} or \code{rtlb}.
}
\usage{
rtlMainFile(fileOut = "main.tex",text="",sweave=FALSE)
}

\arguments{
  \item{fileOut}{\code{characters}; name of the main document.}
  \item{text}{text to be included in the LaTeX document, between the
    '\\begin\{document\}' and '\\end\{document\}' macros.
  }
  \item{sweave}{\code{logical}; if \code{TRUE}, the main document is
    designed to be compiled with \code{Sweave}.}
}

\details{
  \code{rtlMainFile} generates a LaTeX master file or a Sweave file ready to include some
  files built by \code{rtlu} or \code{rtlb}.
}
\value{
  A LaTeX or a Sweave file ("main.tex" by default).
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

\section{English correction}{
  Jean-Marc Chamot \cr
  \email{jchamot@u-paris10.fr}\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre\cr
}


\references{LaTeX web site \url{http://www.latex-project.org/}\cr
Data are available on line: \url{http://christophe.genolini.free.fr/EPO/EPO2007-Fraude.php}}



\seealso{
  \code{\link{rtlu}},
  \code{\link{rtlb}},
  \code{\link{r2lh-package}},
  \code{\link{examCheating}},
  \code{\link{Sweave}},
  \code{\link[Hmisc]{latex}}
}

\examples{
 # # # # # # # # # # # # # # # # # # #
#   R to LaTeX, Main file generation  #
 #             Examples              #
  #           rtlMainFile           #
   # # # # # # # # # # # # # # # # #

### Creates a Sweave file
text <- "
\\\\maketitle
\\\\tableofcontents

<<>>=
data(examCheating)
@

\\\\section{Univariate analysis}

<<>>=
rtlu(examCheating$CheatScore,fileOut='ExamCheat-univ1.tex')
@
\\\\input{ExamCheat-univ1.tex}


\\\\section{Bivariate analysis}

<<>>=
#rtlb(examCheating$CheatScore~examCheating$Sexe,fileOut='ExamCheat-biv1.tex')
@
\\\\input{ExamCheat-biv1.tex}
"

if(!file.exists("example/sweave",recursive=TRUE)){dir.create("example/sweave",recursive=TRUE)}else{}
setwd("example/sweave")

rtlMainFile(fileOut="main.Rnw",text=text,sweave=TRUE)
Sweave("main.Rnw")
setwd("../..")
}


\keyword{print}        % Printing
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
