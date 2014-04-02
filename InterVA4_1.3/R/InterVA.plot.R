Population.summary<-function (va, top = NULL, va.top3 = FALSE, noplot = FALSE, type="both",  min.prob = 0.01, ... ) {
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
	if(!va.top3){
        for(i in 1:length(va)){
            if(is.null(va[[i]][14])) next
            this.dist <- unlist(va[[i]][14])
            cutoff <- this.dist[order(this.dist, decreasing = TRUE)[top]]
            undeter <- undeter + sum(this.dist[which(this.dist < cutoff)])
            
            this.dist[which(this.dist < cutoff)] <- 0
            if(!is.null(va[[i]][14])) dist <- dist + this.dist
        }        
    }else{
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