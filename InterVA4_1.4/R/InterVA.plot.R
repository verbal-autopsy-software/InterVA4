InterVA.summary <- function(va){
    data("causetext", envir = environment())
    causetext <- get("causetext", envir  = environment())
    ## Check if there is a valid va object
    if(length(va) < 1){
        cat("No va object found")
        return
    }
    ## Initialize the population distribution
    dist <- NULL
    for(i in 1:length(va)){
        if(!is.null(va[[i]][14])){  
            dist <- rep(0, length(unlist(va[[i]][14]))) 
            break
        }
    } 
    undeter <- 0

    ## pick not simply the top 3 causes, but the top 3 causes reported by InterVA
    for(i in 1:length(va)){
        if(is.null(va[[i]][14])) next
        this.dist <- unlist(va[[i]][14])
        if(max(this.dist) < 0.4){
          undeter <- undeter + sum(this.dist)  
        }else{
            cutoff.3 <- this.dist[order(this.dist, decreasing = TRUE)[3]]
            cutoff.2 <- this.dist[order(this.dist, decreasing = TRUE)[2]]
            cutoff.1 <- this.dist[order(this.dist, decreasing = TRUE)[1]]
            cutoff <- min(max(cutoff.1 * 0.5 , cutoff.3), max(cutoff.1 * 0.5 , cutoff.2))

            undeter <- undeter + sum(this.dist[which(this.dist < cutoff)])
            this.dist[which(this.dist < cutoff)] <- 0
            if(!is.null(va[[i]][14])) dist <- dist + this.dist
        }
    } 
    ## Normalize the probability for CODs
    if(undeter > 0){
        dist.cod <- c(dist[4:63], undeter)
        dist.cod <- dist.cod/sum(dist.cod)
        names(dist.cod)<-c(causetext[4:63,2], "Undetermined")
    }else{
        dist.cod <- dist[4:63]/sum(dist[4:63])
        names(dist.cod)<-causetext[4:63,2]
    } 

    return(dist.cod)  
}

#' Summarize and plot a population level distribution of va probabilities.
#' 
#' The function takes input of a list of va object and produces a summary plot
#' for the population distribution.
#' 
#' 
#' @param va The list of va object to summarize.
#' @param top Integer indicating how many causes from the top need to go into
#' summary. The rest of the probabilities goes into an extra category
#' "Undetermined".  When set to NULL, default is all causes to be considered.
#' This is only used when \code{va.top3} set to "FALSE".
#' @param va.top3 If it is set to "TRUE", only the top 3 causes reported by
#' InterVA4 is calculated into CSMF. The rest of probabilities goes into an
#' extra category "Undetermined". Default set to "FALSE".
#' @param noplot A logical value indicating whether the plot will be shown. If
#' it is set to "TRUE", only the CSMF will be returned.
#' @param min.prob The minimum probability that is to be plotted in bar chart,
#' or to be labeled in pie chart.
#' @param type An indicator of the type of chart to plot.  "pie" for pie chart;
#' "bar" for bar chart and "both" for both.
#' @param ... Arguments to be passed to/from graphic function
#' \code{\link[graphics]{barplot}}, \code{\link[graphics]{pie}}, and more
#' graphical paramters (see \code{\link[graphics]{par}}). They will affect the
#' main title, size and font of labels, and the radius of the pie chart.
#' @return \item{dist.cod}{The population probability of CODs.}
#' @author Zehang LI, Tyler McCormick, Sam Clark
#' @keywords interVA
#' @examples
#' 
#' data(SampleInput)
#' sample.output <- InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test", 
#'     filename = "VA_result", output = "extended", append = FALSE)
#' 
#' ## Get CSMF without plots
#' population.summary <- Population.summary(sample.output$VA, noplot = TRUE)
#' 
#' 
#' ## Get CSMF by considering only top 3 causes for each death.
#' population.summary <- Population.summary(sample.output$VA, top = 3, noplot = TRUE)
#' 
#' ## Get CSMF by considering only top 3 causes reported by InterVA.  Note
#' ## it's different from using all top 3 causses, since they may not all be
#' ## reported
#' population.summary <- Population.summary(sample.output$VA, va.top3 = TRUE, 
#'     noplot = TRUE)
#' 
#' ## Population level summary using pie chart
#' population.summary <- Population.summary(sample.output$VA, type = "pie", 
#'     min.prob = 0.01, main = "population COD distribution using pie chart", 
#'     clockwise = FALSE, radius = 0.7, cex = 0.7, cex.main = 0.8)
#' 
#' ## Population level summary using bar chart
#' population.summary <- Population.summary(sample.output$VA, type = "bar", 
#'     min.prob = 0.01, main = "population COD distribution using bar chart", 
#'     cex.main = 1)
#' 

Population.summary<-function (va, top = NULL, InterVA = FALSE, noplot = FALSE, type="both",  min.prob = 0.01, ... ) {
	# data(causetext)
    data("causetext", envir = environment())
    causetext <- get("causetext", envir  = environment())
    ## Check if there is a valid va object
	if(length(va) < 1){
		cat("No va object found")
		return
	}
    ## Initialize the population distribution
    dist <- NULL
    for(i in 1:length(va)){
        if(!is.null(va[[i]][14])){	
	        dist <- rep(0, length(unlist(va[[i]][14])))	
	        break
        }
    } 
    ## determine how many causes from top need to be summarized
    if(is.null(top)) top <- 60
    undeter <- 0

    if(is.null(dist)){cat("No va probability found in input"); return}   
    ## Add the probabilities together
	if(!InterVA){
        for(i in 1:length(va)){
            if(is.null(va[[i]][14])) {undeter = undeter + 1; next}
            this.dist <- unlist(va[[i]][14])
            cutoff <- this.dist[order(this.dist, decreasing = TRUE)[top]]
            undeter <- undeter + sum(this.dist[which(this.dist < cutoff)])
            
            this.dist[which(this.dist < cutoff)] <- 0
            if(!is.null(va[[i]][14])) dist <- dist + this.dist
        }  
            ## Normalize the probability for CODs
        if(undeter > 0){
            dist.cod <- c(dist[4:63], undeter)
            dist.cod <- dist.cod/sum(dist.cod)
            names(dist.cod)<-c(causetext[4:63,2], "Undetermined")
        }else{
            dist.cod <- dist[4:63]/sum(dist[4:63])
            names(dist.cod)<-causetext[4:63,2]
        }      
    }else{
        dist.cod <- InterVA.summary(va)   
    }


    ## Check if there is CODs above the minimum cut-off for prob
    if(max(dist.cod) < min.prob){
        cat("No COD larger than the minimum probability cut off line")
        return
    }
    if(noplot){
    		return(dist.cod)
    }
    ## Make pie plot upon request
    if( type == "pie" || type == "both"){
        dev.new()
        dist.cod.sort <- sort(dist.cod, decreasing=TRUE)
        pie.color <- grey.colors(length(dist.cod.sort[dist.cod.sort >= min.prob]))
        pie.color.left <- rep(pie.color[length(pie.color)], length(dist.cod.sort[dist.cod.sort < min.prob]))
        pie.color <- c(pie.color, pie.color.left)
        pie(dist.cod.sort, col = pie.color,labels = names(dist.cod.sort)[dist.cod.sort > min.prob], ...)
        
    }
    ## Make bar plot upon request
    if( type == "bar"|| type == "both"){
        dev.new()
        dist.cod.min <- dist.cod[dist.cod > min.prob ]
        dist.cod.min <- sort(dist.cod.min, decreasing = FALSE)
        par(las = 2)
        par(mar = c(5,15,4,2))
        bar.color <- grey.colors(length(dist.cod.min))
        bar.color <- rev(bar.color)
        barplot(dist.cod.min , horiz = TRUE,names.arg = names(dist.cod.min), col = bar.color, cex.names=0.8, xlab = "Probability", ...)
    }
    ## Save the population distribution
    dist.cod
	
}






#' Plot a individual level distribution of va probabilities.
#' 
#' The function takes input of a single va object and produces a summary plot
#' for it.
#' 
#' 
#' @param va A va object
#' @param min.prob The minimum probability that is to be plotted in bar chart,
#' or to be labeled in pie chart.
#' @param type An indicator of the type of chart to plot.  "pie" for pie chart;
#' "bar" for bar chart and "both" for both.
#' @param ... Arguments to be passed to/from graphic function
#' \code{\link[graphics]{barplot}}, \code{\link[graphics]{pie}}, and more
#' graphical paramters (see \code{\link[graphics]{par}}). They will affect the
#' main title, size and font of labels, and the radius of the pie chart.
#' @seealso \code{\link{Population.summary}}
#' @keywords InterVA
#' @examples
#' 
#' data(SampleInput)
#' sample.output <- InterVA(SampleInput, HIV = "h", Malaria = "v", directory = "VA test", 
#'     filename = "VA_result", output = "extended", append = FALSE)
#' 
#' ## Individual level summary using pie chart
#' InterVA.plot(sample.output$VA[[7]], type = "pie", min.prob = 0.01, 
#'     main = "1st sample VA analysis using pie chart", clockwise = FALSE, 
#'     radius = 0.6, cex = 0.6, cex.main = 0.8)
#' 
#' 
#' ## Individual level summary using bar chart
#' InterVA.plot(sample.output$VA[[7]], type = "bar", min.prob = 0.01, 
#'     main = "2nd sample VA analysis using bar chart", cex.main = 0.8)
#' 
InterVA.plot <- function(va, type="both", min.prob = 0.01, ... ){
    # data(causetext)
    data("causetext", envir = environment())
    causetext <- get("causetext", envir  = environment())
    
    ## Check if there is a valid va object
	if(length(va) < 1){
		cat("No va object found")
		return
	}
    ## Find the probability distribution
	dist <- unlist(va[14])
    dist.cod <- dist[4:63]/sum(dist[4:63])
    ## Check if there is CODs above the minimum cut-off for prob
    if(max(dist.cod) < min.prob){
        cat("No COD larger than the minimum probability cut off line")
        return
    }
    names(dist.cod)<-causetext[4:63,2]
    ## Make pie plot upon request    
    if( type == "pie" || type == "both"){
        dev.new()
        dist.cod.sort <- sort(dist.cod, decreasing=TRUE)
        pie.color <- grey.colors(length(dist.cod.sort[dist.cod.sort >= min.prob]))
        pie.color.left <- rep(pie.color[length(pie.color)], length(dist.cod.sort[dist.cod.sort < min.prob]))
        pie.color <- c(pie.color, pie.color.left)
        pie(dist.cod.sort, col = pie.color,labels = names(dist.cod.sort)[dist.cod.sort > min.prob], ...)
        
    }
    ## Make bar plot upon request
    if( type == "bar"|| type == "both"){
        dev.new()
        dist.cod.min <- dist.cod[dist.cod > min.prob ]
        dist.cod.min <- sort(dist.cod.min, decreasing = FALSE)
        par(las = 2)
        par(mar = c(5,15,4,2))
        bar.color <- grey.colors(length(dist.cod.min))
        bar.color <- rev(bar.color)
        barplot(dist.cod.min , horiz = TRUE,names.arg = names(dist.cod.min), col = bar.color, cex.names=0.8, xlab = "Probability", ...)
    }

}
