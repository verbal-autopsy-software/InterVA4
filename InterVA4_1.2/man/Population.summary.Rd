\name{Population.summary}
\alias{Population.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize and plot a population level distribution of va probabilities.
}
\description{
The function takes input of a list of va object and produces a summary plot for the population distribution.}
\usage{
Population.summary(va, noplot = FALSE, type="both", min.prob = 0.01, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{va}{
The list of va object to summarize.}
  \item{noplot}{
A logical value indicating whether the plot will be shown. If it is set to "TRUE", only the CSMF will be returned.  	
  }
  \item{min.prob}{
The minimum probability that is to be plotted in bar chart, or to be labeled in pie chart.
}
  \item{type}{ An indicator of the type of chart to plot. "pie" for pie chart; "bar" for bar chart and "both" for both.}
  \item{...}{ Arguments to be passed to/from graphic function \code{\link[graphics]{barplot}}, \code{\link[graphics]{pie}}, and more graphical paramters (see \code{\link[graphics]{par}}). They will affect the main title, size and font of labels, and the radius of the pie chart. }
}


\value{
\item{dist.cod}{The population probability of CODs.}

}

\author{
Zehang LI, Tyler McCormick, Sam Clark
}

\examples{
data(SampleInput)
sample.output<-InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test",
filename = "VA_result", output = "extended", append=FALSE)

## Get CSMF without plots
population.summary <- Population.summary(sample.output$VA, noplot = TRUE)

## Population level summary using pie chart
population.summary <- Population.summary(sample.output$VA, type = "pie", min.prob = 0.01, 
main = "population COD distribution using pie chart", clockwise = FALSE, 
radius = 0.7, cex = 0.7, cex.main = 0.8)

## Population level summary using bar chart
population.summary <- Population.summary(sample.output$VA, type = "bar", min.prob = 0.01,
main = "population COD distribution using bar chart", cex.main = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interVA}

