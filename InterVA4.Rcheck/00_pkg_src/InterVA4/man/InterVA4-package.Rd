\name{InterVA4-package}
\alias{InterVA4-package}
\alias{InterVA4}
\docType{package}
\title{
Perform InterVA4 algorithm and provide graphical summarization of COD distribution.}
\description{
Computes individual cause of death and population cause-specific mortality fractions using the InterVA4 algorithm. Provides a simple graphical representation of the result.
}
\details{
\tabular{ll}{
Package: \tab InterVA4\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-12-18\cr
License: \tab GPL-2\cr
}

}
\author{
Zehang LI, Tyler McCormick, Sam Clark

Maintainer: Zehang LI <lizehang@uw.edu>

}
\references{
http://www.interva.net/}

\keyword{ InterVA}

\examples{
data(SampleInput)
sample.output<-InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test",
filename = "VA_result", output = "extended", append=FALSE)
}

