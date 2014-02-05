\name{InterVA}
\alias{InterVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Provide InterVA4 analysis on the data input.
}
\description{
This function implements the algorithm in the InterVA4 software.  It produces individual cause of death and population cause-specific mortality fractions.
}
\usage{
InterVA(Input, HIV, Malaria, directory = NULL, filename = "VA_result", 
        output = "classic", append = FALSE, replicate = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Input}{
A matrix input, or data read from csv files in the same format as required by InterVA4. Sample input is included as data(SampleInput).
}
  \item{HIV}{
An indicator of the level of prevalence of HIV. The input should be one of the following: "h"(high),"l"(low), or "v"(very low). 
}
  \item{Malaria}{
An indicator of the level of prevalence of Malaria. The input should be one of the following: "h"(high),"l"(low), or "v"(very low).
}
  \item{directory}{
The directory to store the output from InterVA4. It should either be an existing valid directory, or a new folder to be created. If no path is given, the current working directory will be used.  	
}
  \item{filename}{
The filename the user wish to save the output. No extension needed. The output is in .csv format by default. 
}
  \item{output}{
"classic": The same deliminated output format as InterVA4; or "extended": deliminated output followed by full distribution of cause of death proability. 
}
  \item{append}{
A logical value indicating whether or not the new output should be appended to the existing file.
}
  \item{replicate}{
A logical value indicating whether or not the calculation should contain a possible bug in original InterVA4 implementation.  If replicate=T, then the output values will be exactly as they would be from calling the InterVA4 program.  	
  }
}
\details{
InterVA performs the same tasks as the InterVA4. The output is saved in a .csv file specified by user. The calculation is based on the conditional and prior distribution of 68 CODs. The function also could save the full probability distibution of each individual to file. All information about each individual is saved to a va class object.  
}
\value{
\item{ID }{identifier from batch (input) file}
\item{MALPREV }{selected malaria prevalence}
\item{HIVPREV }{selected HIV prevalence}
\item{PREGSTAT }{most likely pregnancy status}
\item{PREGLIK }{likelihood of PREGSTAT}
\item{PRMAT }{	likelihood of maternal death}
\item{INDET }{indeterminate outcome}
\item{CAUSE1 }{	most likely cause}
\item{LIK1 }{	likelihood of 1st cause}
\item{CAUSE2 }{	second likely cause}
\item{LIK2 	}{	likelihood of 2nd cause}
\item{CAUSE3 }{	third likely cause}
\item{LIK3 	}{	likelihood of 3rd cause}
\item{wholeprob}{ full distribution of causes of death}

}
\references{
http://www.interva.net/
}
\author{
Zehang Li, Tyler McCormick, Sam Clark
}

\seealso{
\code{\link{InterVA.plot}}
}
\examples{
data(SampleInput)
sample.output<-InterVA(SampleInput, HIV = "h", Malaria = "l", directory = "VA test",
filename = "VA_result", output = "extended", append = FALSE, replicate = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ InterVA }

