\name{InterVA.summary}
\alias{InterVA.summary}
\title{Summarize population level cause-specific mortality fraction as InterVA4.}
\usage{
InterVA.summary(va)
}
\arguments{
  \item{va}{The list of va object to summarize.}
}
\value{
\item{dist.cod}{The cause-specific mortality fraction (including undetermined category).}
}
\description{
The function takes input of a list of va object and
calculates the cause-specific mortality fraction.
}
\examples{
data(SampleInput)
sample.output <- InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test", 
    filename = "VA_result", output = "extended", append = FALSE)

## Get CSMF without plots
csmf<- InterVA.summary(sample.output$VA)
}
\author{
Zehang LI, Tyler McCormick, Sam Clark
}
\seealso{
\code{\link{Population.summary}}
}
\keyword{interVA}
