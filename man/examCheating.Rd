\name{examCheating}
\alias{examCheating}

\docType{data}
\title{~ Data: Exam Cheating at French University ~}
\description{
  This data set results from an enquiry led by some second year
  Students from the Paris X - Nanterre University. They built up a
  questionnaire to measure fraud during exams at the University. This
  is not a valid questionnaire, just an exercise done by some students
  on a topic they chose -to make learning statistics more interactive
  (more fun ?)-.
}

\usage{
data(examCheating)
}
\format{
  A data frame with 314 observations on the following 20 variables.
  \describe{
    \item{\code{Id}}{Line identification}
    \item{\code{Age}}{\emph{What is your age ?}}
    \item{\code{Gender}}{\emph{What is your gender ?}}
    \item{\code{YearOfStudy}}{\emph{What is your studying year ?}\cr
      Note: In European University, the studies are
      split in two cycles. The first one called "Licence" lasts three years (L1, L2
      then L3). The second one called "Master" (M1 then M2) comes
      after the licence and takes two years.}
    \item{\code{Field}}{\emph{What do you study ?}\cr
      Note:\code{LEA} is "Foreigh langage",
      \code{SEGMI} is "Math and Economics", \code{SJAP} is "Law", \code{SSA} is "Social and Administrative Sciences",
      \code{STAPS} is "Sports"}
    \item{\code{RepeatingYear}}{\emph{Did you ever take a class twice ?}\cr
      In France, a student who failed at the final exam will be proposed to
      take the same year of study once again.}
    \item{\code{Bac}}{\emph{What grade did you get at Bac ?} \cr
      Bac is a french national exam that takes place between high school
      and university. The possible results are \code{Fail}
      \code{Remedial exam} \code{Pass} \code{Fairly good} \code{Good}
      \code{Very good}  \code{Summa cum laude}}
    \item{\code{Peep}}{\emph{Did you ever peep during exams ?}}
    \item{\code{Whispering}}{\emph{Did you ever whisper to neighbors during exams ?}}
    \item{\code{PaperSwapping}}{\emph{Did you ever swap rough paper during exams ?}}
    \item{\code{CribSheet}}{\emph{Did you ever use crib sheet ?}}
    \item{\code{SMS}}{\emph{Did you ever send SMS during exams ?}}
    \item{\code{LapCopying}}{\emph{Did you ever lap copy during exams ?}}
    \item{\code{PaperKeeping}}{\emph{Did you ever keep your paper ?}}
    \item{\code{PreparingRoom}}{\emph{Did you ever prepare the exam room
	or the toilets before exams ?}}
    \item{\code{ExamStealing}}{\emph{Did you ever steal the exam subject
	before the exam ?}}
    \item{\code{Other}}{\emph{Did you use some other way of cheating ?}}
    \item{\code{BacCheat}}{\emph{Did you cheat during the Bac ?}}
    \item{\code{HighSchoolCheat}}{\emph{Did you cheat during you high school
	studies ?}}
    \item{\code{CheatScore}}{Sum of all the cheating
      variables}{with convention \code{Never=0} \code{Rarely=1}
      \code{Sometimes=2} \code{Often=3} \code{Always=4}}
  }
}


\section{Author}{
  Christophe Genolini \cr
  \email{christophe.genolini@free.fr}\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health \cr
  INSERM U669 / Maison de Solenn / Paris
}

\section{Author (students)}{L2 (2007-2008) groups TD1 and TD2}

\section{English correction}{
  Jean-Marc Chamot \cr
  \email{jchamot@u-paris10.fr}\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}


\references{LaTeX web site \url{http://www.latex-project.org/}\cr
Data are available on line: \url{http://christophe.genolini.free.fr/EPO/EPO2007-Fraude.php}}



\seealso{
  \code{\link{r2lUniv-package}},
  \code{\link{rtlu}},
  \code{\link{rtlMainFile}},
  \code{\link{examCheating}},
  \code{\link{Sweave}},
  \code{\link[Hmisc]{latex}}
}


\examples{
data(examCheating)
str(examCheating)
}
\keyword{datasets}
\keyword{print}        % Printing
\keyword{interface}    % interface to other languages
\keyword{utilities}    % Utilities
\keyword{univar}       % Simple univariate statistics

