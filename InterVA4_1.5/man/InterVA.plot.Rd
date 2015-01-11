\name{InterVA.plot}
\alias{InterVA.plot}
\title{Plot a individual level distribution of va probabilities.}
\usage{
InterVA.plot(va, type = "both", min.prob = 0.01, ...)
}
\arguments{
  \item{va}{A va object}

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
\description{
The function takes input of a single va object and produces
a summary plot for it.
}
\examples{
data(SampleInput)
sample.output <- InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test", 
    filename = "VA_result", output = "extended", append = FALSE)

## Individual level summary using pie chart
InterVA.plot(sample.output$VA[[7]], type = "pie", min.prob = 0.01, 
    main = "1st sample VA analysis using pie chart", clockwise = FALSE, 
    radius = 0.6, cex = 0.6, cex.main = 0.8)


## Individual level summary using bar chart
InterVA.plot(sample.output$VA[[7]], type = "bar", min.prob = 0.01, 
    main = "2nd sample VA analysis using bar chart", cex.main = 0.8)
}
\author{
Zehang LI, Tyler McCormick, Sam Clark
}
\seealso{
\code{\link{Population.summary}}
}
\keyword{interVA}
