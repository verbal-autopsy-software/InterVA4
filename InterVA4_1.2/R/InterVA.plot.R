Population.summary<-function (va, noplot = FALSE, type="both",  min.prob = 0.01, ... ) {
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
    if(is.null(dist)){cat("No va probability found in input"); return}   
    ## Add the probabilities together
	for(i in 1:length(va)){
        if(!is.null(va[[i]][14])) dist <- dist + unlist(va[[i]][14])
    }
    ## Normalize the probability for CODs
    dist.cod <- dist[4:63]/sum(dist[4:63])
    ## Check if there is CODs above the minimum cut-off for prob
    if(max(dist.cod) < min.prob){
        cat("No COD larger than the minimum probability cut off line")
        return
    }
    names(dist.cod)<-causetext[4:63,2]
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