\name{Population.summary}
\alias{Population.summary}
\title{Summarize and plot a population level distribution of va probabilities.}
\usage{
Population.summary(va, top = NULL, InterVA = FALSE, noplot = FALSE, type = "both", 
    min.prob = 0.01, ...)
}
\arguments{
  \item{va}{The list of va object to summarize.}

  \item{top}{Integer indicating how many causes from the
  top need to go into summary. The rest of the
  probabilities goes into an extra category "Undetermined".
  When set to NULL, default is all causes to be considered.
  This is only used when \code{InterVA} set to "FALSE".}

  \item{InterVA}{If it is set to "TRUE", only the top 3
  causes reported by InterVA4 is calculated into CSMF as in InterVA4. The
  rest of probabilities goes into an extra category
  "Undetermined". Default set to "FALSE".}

  \item{noplot}{A logical value indicating whether the plot
  will be shown. If it is set to "TRUE", only the CSMF will
  be returned.}

  \item{min.prob}{The minimum probability that is to be
  plotted in bar chart, or to be labeled in pie chart.}

  \item{type}{An indicator of the type of chart to plot.
  "pie" for pie chart; "bar" for bar chart and "both" for
  both.}

  \item{...}{Arguments to be passed to/from graphic
  function \code{\link[graphics]{barplot}},
  \code{\link[graphics]{pie}}, and more graphical paramters
  (see \code{\link[graphics]{par}}). They will affect the
  main title, size and font of labels, and the radius of
  the pie chart.}
}
\value{
\item{dist.cod}{The population probability of CODs.}
}
\description{
The function takes input of a list of va object and
produces a summary plot for the population distribution.
}
\examples{
data(SampleInput)
sample.output <- InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test", 
    filename = "VA_result", output = "extended", append = FALSE)

## Get CSMF without plots
population.summary <- Population.summary(sample.output$VA, noplot = TRUE)


## Get CSMF by considering only top 3 causes for each death.
population.summary <- Population.summary(sample.output$VA, top = 3, noplot = TRUE)

## Get CSMF by considering only top 3 causes reported by InterVA.  
## This is equivalent to using InterVA.summary() command Note that
## it's different from using all top 3 causses, since they may not 
## all be reported 
population.summary <- Population.summary(sample.output$VA, InterVA = TRUE, 
    noplot = TRUE)

## Population level summary using pie chart
population.summary <- Population.summary(sample.output$VA, type = "pie", 
    min.prob = 0.01, main = "population COD distribution using pie chart", 
    clockwise = FALSE, radius = 0.7, cex = 0.7, cex.main = 0.8)

## Population level summary using bar chart
population.summary <- Population.summary(sample.output$VA, type = "bar", 
    min.prob = 0.01, main = "population COD distribution using bar chart", 
    cex.main = 1)
}
\author{
Zehang LI, Tyler McCormick, Sam Clark
}
\seealso{
\code{\link{InterVA.summary}}
}
\keyword{interVA}
