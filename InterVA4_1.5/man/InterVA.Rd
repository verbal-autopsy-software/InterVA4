\name{InterVA}
\alias{InterVA}
\title{Provide InterVA4 analysis on the data input.}
\usage{
InterVA(Input, HIV, Malaria, directory = NULL, filename = "VA_result", 
    output = "classic", append = FALSE, groupcode = FALSE, 
    replicate = FALSE, write = TRUE)
}
\arguments{
  \item{Input}{A matrix input, or data read from csv files
  in the same format as required by InterVA4. Sample input
  is included as data(SampleInput).}

  \item{HIV}{An indicator of the level of prevalence of
  HIV. The input should be one of the following:
  "h"(high),"l"(low), or "v"(very low).}

  \item{Malaria}{An indicator of the level of prevalence of
  Malaria. The input should be one of the following:
  "h"(high),"l"(low), or "v"(very low).}

  \item{directory}{The directory to store the output from
  InterVA4. It should either be an existing valid
  directory, or a new folder to be created. If no path is
  given, the current working directory will be used.}

  \item{filename}{The filename the user wish to save the
  output. No extension needed. The output is in .csv format
  by default.}

  \item{output}{"classic": The same deliminated output
  format as InterVA4; or "extended": deliminated output
  followed by full distribution of cause of death
  proability.}

  \item{append}{A logical value indicating whether or not
  the new output should be appended to the existing file.}

  \item{replicate}{A logical value indicating whether or
  not the calculation should replicate original InterVA4
  software exactly.If replicate = F, causes with small
  probability are not dropped out of calculation in
  intermediate steps, and a possible bug in original
  InterVA4 implementation is fixed.  If replicate=T, then
  the output values will be exactly as they would be from
  calling the InterVA4 program.}

  \item{groupcode}{A logical value indicating whether or
  not the group code will be included in the output
  causes.}

  \item{write}{A logical value indicating whether or
  not the output (including errors and warnings) will be saved to file.}
}
\value{
\item{ID }{identifier from batch (input) file}
\item{MALPREV }{selected malaria prevalence} \item{HIVPREV
}{selected HIV prevalence} \item{PREGSTAT }{most likely
pregnancy status} \item{PREGLIK }{likelihood of PREGSTAT}
\item{PRMAT }{ likelihood of maternal death} \item{INDET
}{indeterminate outcome} \item{CAUSE1 }{ most likely cause}
\item{LIK1 }{ likelihood of 1st cause} \item{CAUSE2 }{
second likely cause} \item{LIK2 }{ likelihood of 2nd cause}
\item{CAUSE3 }{ third likely cause} \item{LIK3 }{
likelihood of 3rd cause} \item{wholeprob}{ full
distribution of causes of death}
}
\description{
This function implements the algorithm in the InterVA4
software.  It produces individual cause of death and
population cause-specific mortality fractions.
}
\details{
InterVA performs the same tasks as the InterVA4. The output
is saved in a .csv file specified by user. The calculation
is based on the conditional and prior distribution of 68
CODs. The function also could save the full probability
distibution of each individual to file. All information
about each individual is saved to a va class object.

Be careful if the input file does not match InterVA input
format strictly. The function will run normally as long as
the number of symptoms are correct. Any inconsistent
symptom names will be printed in console as warning. If
there's wrong match of symptom from warning, please change
in the input to correct orders.
}
\examples{
data(SampleInput)
## to get easy-to-read version of causes of death make sure the column
## orders match interVA4 standard input this can be monitored by checking
## the warnings of column names

sample.output1 <- InterVA(SampleInput, HIV = "h", Malaria = "l", directory = "VA test", 
    filename = "VA_result", output = "extended", append = FALSE, replicate = TRUE)

## to get causes of death with group code for further usage
sample.output2 <- InterVA(SampleInput, HIV = "h", Malaria = "l", directory = "VA test", 
    filename = "VA_result_wt_code", output = "classic", append = FALSE, 
    replicate = TRUE, groupcode = TRUE)
}
\author{
Zehang Li, Tyler McCormick, Sam Clark
}
\references{
http://www.interva.net/
}
\seealso{
\code{\link{InterVA.plot}}
}
\keyword{InterVA}
