
#' Can read file
#'
#' Determines whether the given file exists and is readable
#' @param filename the name of the file
#' @return boolean
#' @export
#' @examples
#' \dontrun{
#' canRead("foobar.txt")
#' }
canRead <- function(filename) file.access(filename,mode=4) == 0


#' Get CLI argument
#' 
#' Retrieves a user-supplied argument command-line argument
#' with a given name. Argument syntax: name=value
#' 
#' @param name The name of the command line argument
#' @param default If no value was given by the user, default to this
#' @param required Whether the argument is required or not. If TRUE,
#'   an error is raised when no value was provided.
#' @export
#' @examples
#' \dontrun{
#' infile <- getArg("inputFile",required=TRUE)
#' userIQ <- getArg("userIQ",default=0)
#' }
getArg <- function(name, default=NULL, required=FALSE) {

	if (length(commandArgs(TRUE)) == 0) {
		if (required) {
			stop("Required argument:",name)
		} else {
			return(default)
		}
	}

	#tabulate arguments by name
	argTable <- do.call(rbind,strsplit(commandArgs(TRUE),"="))
	#get index of argument with given name
	i <- which(argTable[,1] == name)


	if (length(i) == 0) {
		#return default value if no matching arguments are found.
		if (required) {
			stop("Required argument:",name)
		} else {
			return(default)
		}
	} else if (length(i) > 1) {
		#if multiple matches are found, throw error message.
		stop("Multiple values for", name, "found!")
	} else {
		#if everything checks out, return the argument value
		return(argTable[i,2])
	}
}

#' Create new Counter
#'
#' This constructor method creates an object that can count 
#' occurrences of different items. It allows importing and exporting
#' of the counter status in string form.
#' 
#' The object has the following methods:
#' \itemize{
#'  \item \code{inc(id)}: Increase the counter for item with \code{id} by 1.
#'  \item \code{add(id,x)}: Add \code{x} occurrences for the item with \code{id}.
#'  \item \code{get(id)}: Get the number of occurrences seen for item \code{id}.
#'  \item \code{ls(id)}: List all counts for all items by id.
#'  \item \code{export(id)}: Exports the counter state to a string that can be saved or logged.
#'  \item \code{import.add(str)} Imports a previous counter state from the string \code{str} 
#'      and adds it to the current counts.
#' }
#' 
#' @return An object of type \code{yogicounter}.
#' @export
#' @examples
#' cn <- new.counter()
#' cn$inc("foo")
#' cn$inc("bar")
#' cn$add("foo",6)
#' cn$get("foo")
#' # 7
#' cn$ls()
#' # foo 7
#' # bar 1
#' cn$export()
#' # foo=7,bar=1
new.counter <- function() {

	a <- list()

	###
	# Add x occurrences to item id
	#
	add <- function(id,x) {
		if (!is.character(id)) {
			stop("Illegal argument:",id)
		}
		if (is.null(a[[id]])) {
			a[[id]] <<- x
		} else {
			a[[id]] <<- a[[id]] + x
		}
	}

	# increase counter for item id by 1
	inc <- function(id) add(id,1)

	# get the counter state for id
	get <- function(id) a[[id]]

	# list counts for all ids
	ls <- function() a

	# export counter state as a string
	export <- function() {
		paste(lapply(names(a), function(id) paste(id,"=",a[[id]],sep="") ), collapse=",")
	}

	# import counter state from string
	import.add <- function(strs) { 
		lapply(strsplit(strs,","), function(eqs) {
			lapply(strsplit(eqs,"="), function(vals) {
				add(vals[[1]],as.numeric(vals[[2]]))
			})
		})
		invisible()
	}

	structure(
		list(
			inc = inc,
			add = add,
			get = get,
			ls = ls,
			export = export,
			import.add = import.add
		),
		class="yogicounter"
	)
}

#' 3D-bind matrices
#' 
#' Binds matrices of same size together to a 3D array, analogously
#' to cbind and rbind.
#' 
#' @param ... Any number of matrices of the same size
#' @return A 3D array of the bound matrices
#' @export
zbind <- function(...) {
	x <- list(...)
	y <- array(0,dim=c(nrow(x[[1]]),ncol(x[[1]]),length(x)),dimnames=dimnames(x[[1]]))
	for (i in 1:length(x)) y[,,i] <- x[[i]]
	y
}

#' Extract regex groups (local)
#' 
#' Locally excise regular expression groups from string vectors.
#' I.e. only extract the first occurrence of each group within each string.
#' 
#' @param x A vector of strings from which to extract the groups.
#' @param re The regular expression defining the groups
#' @return A \code{matrix} containing the group contents, 
#'      with one row for each element of x and one column for each group.
#' @keywords regular expression groups
#' @export
extract.groups <- function(x, re) {
	matches <- regexpr(re,x,perl=TRUE)
	start <- attr(matches,"capture.start")
	end <- start + attr(matches,"capture.length") - 1
	do.call(cbind,lapply(1:ncol(start), function(i) {
		sapply(1:nrow(start),function(j){
			if (start[j,i] > -1) substr(x[[j]],start[j,i],end[j,i]) else NA
		})
	}))
}

#' Extract regex groups (global)
#' 
#' Globally excise regular expression groups from string vectors.
#' I.e. only extract the all occurrences of each group within each string.
#' 
#' @param x A vector of strings from which to extract the groups.
#' @param re The regular expression defining the groups
#' @return A \code{list} of \code{matrix}'s containing the group contents, 
#'      with one list item for every element of x, and with each matrix 
#'      containing one column for each group and one row for each occurrence
#'      of the pattern.
#' @keywords regular expression groups
#' @export
global.extract.groups <- function(x,re) {
    all.matches <- gregexpr(re,x,perl=TRUE)
    mapply(function(matches,x) {
        start <- attr(matches,"capture.start")
        end <- start + attr(matches,"capture.length") - 1
        apply(zbind(start,end),c(1,2),function(pos) substr(x,pos[[1]],pos[[2]]) )
    },matches=all.matches,x=x,SIMPLIFY=FALSE)
}




#' Get i'th rank from list
#' 
#' Retieve the i'th ranked item from a numerical vector
#' 
#' @param values a numerical vector
#' @param i the rank
#' @param high whether to rank by highest or lowest values.
#' @return the ith ranked value
#' @export
#' @examples
#' vals <- rnorm(100,0,1)
#' ith.rank(vals,4)
ith.rank <- function(values, i, high=TRUE) sort(values,decreasing=high)[[i]]


#' Matthew's correlation coefficient (MCC)
#' 
#' Calculate Matthew's correlation coeffient (MCC). 
#' See \url{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}
#' 
#' @param t the score threshold
#' @param scores vector of scores for each measured item
#' @param truth \code{logical} vector classifying each item as a 
#'    member of the hidden true or false classes
#' @return a vector listing the MCC value, the precision, and the recall
#' @export
#' @examples
#' patientHasDisease <- sample(c(TRUE,FALSE),100,replace=TRUE)
#' patientDiganosticScore <- sapply(patientHasDisease,
#'    function(d) if (d) rnorm(1,20,3) else rnorm(1,18,3)
#' )
#' mccval <- mcc(21,patientDiganosticScore,patientHasDisease)
mcc <- function(t, scores, truth) {

	# exclude <- is.na(scores) | is.na(truth)
	# scores <- scores[-exclude]
	# truth <- truth[-exclude]

	.truth <- truth == 1
	.calls <- scores >= t

	tp <- sum(.truth & .calls)
	tn <- sum(!.truth & !.calls)
	fp <- sum(.calls & !.truth)
	fn <- sum(.truth & !.calls)

	# mcc <- (tp * tn - fp * fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
	#this formula prevents integer overflow errors
	mcc <- exp( log(tp*tn - fp*fn) - ( log(tp+fp) + log(tp+fn) + log(tn+fp) + log(tn+fn) )/2 )
	prec <- tp/(tp+fp)
	recall <- tp/(tp+fn)
	c(mcc=mcc,prec=prec,recall=recall)
}


#' Convert row-bound lists to data.frame
#' 
#' Running \code{rbind} on lists with the same element names
#' yields a datastructure very similar to a \code{data.frame}, but does not
#' provide the same full functionality. This function converts such 
#' objects to a real dataframe.
#' @param x the result of the rbind call.
#' @return a \code{data.frame}
#' @export
#' @examples
#' x <- rbind(list(a=1,b="foo"),list(a=2,b="bar"))
#' y <- to.df(x)
to.df <- function(x) {
	if (is.null(x)) return(NULL)
	y <- lapply(1:ncol(x), function(col) {
		unlist(x[,col])
	})
	names(y) <- colnames(x)
	as.data.frame(y,stringsAsFactors=FALSE)
}

#' Convert list of lists into data.frame
#' 
#' The list of lists should only contain lists with the same element names.
#' @param x the list of lists
#' @return a \code{data.frame}
#' @export
#' @examples
#' x <- as.df(list(list(a=1,b="foo"),list(a=2,b="bar")))
as.df <- function(x) {
	df <- as.data.frame(lapply(1:length(x[[1]]),function(i) sapply(x,`[[`,i) ), stringsAsFactors=FALSE)
	colnames(df) <- names(x[[1]])
	df
}

#' Remove infinite and NA values 
#' 
#' Removes infinite and NA values from vectors, lists, matrices and data.frames.
#' 
#' Warning: If the given object is matrix or data.frame, any row containing infinite or NA
#' values is removed entirely. All columns must be numeric.
#' @param x the object to which the function is applied
#' @return the same object, with NAs and infinite values removed
#' @export
#' @examples
#' fin(c(1,2,NA,3))
#' fin(data.frame(a=c(1,2,NA,3),b=c(4,5,6,7)))
fin <- function(x) {
	if (inherits(x,"data.frame") || inherits(x,"matrix")) {
		x[apply(x,1,function(.x) all(!is.na(.x) & is.finite(.x))),]
	} else {
		x[!is.na(x) & is.finite(x)]
	}
}


#' Convert from Quadrant to Coordinate adress
#' 
#' Converts address tags for 384-well plates from the quadrant system (e.g. C_A08)
#' to the raw coordinate system (e.g. B15). 
#' @param x a quadrant coordinate (e.g. C_A08) (do not directly use on vectors!)
#' @return the raw plate coordinate
#' @export
#' @examples
#' q2c("C_A08")
q2c <- function(x) {
	q <- which(LETTERS==substr(x,1,1))
	r <- which(LETTERS==substr(x,3,3))
	c <- as.numeric(substr(x,4,nchar(x)))
	.r <- r*2 - (q<3)
	.c <- c*2 - q%%2
	paste(LETTERS[[.r]],sprintf("%02d",.c),sep="")
}

#' Convert from Raw coordinate to Quadrant address
#' 
#' Converts address tags for 384-well plates from the 
#' raw coordinate system (e.g. B15) to the quadrant system (e.g. C_A08). 
#' @param x a raw coordinate (e.g. B15) (do not directly use on vectors!)
#' @return the quadrant plate coordinate
#' @export
#' @examples
#' c2q("B15")
c2q <- function(x) {
	r <- which(LETTERS==substr(x,1,1))
	c <- as.numeric(substr(x,2,nchar(x)))
	.r <- ceiling(r/2)
	.c <- ceiling(c/2)
	.q <- ((r-1)%%2)*2 + (c-1)%%2+1
	paste(LETTERS[[.q]],"_",LETTERS[[.r]],sprintf("%02d",.c),sep="")
}


#' Set of subsets
#' 
#' Generates the set of all possible subsets for a given list or vector
#' @param l a list or vector
#' @return a list of lists containing all possible subsets of the input
#' @export
#' @examples
#' combo(1:4)
combo <- function(l) {
	do.call(c,lapply(1:length(l),function(n){
		tab <- combn(l,n)
		lapply(1:ncol(tab),function(i)tab[,i])
	}))
}


#' Deconstruct a string into single characters
#' 
#' Converts a single character string into a a vector of individual characters
#' @param str the input string
#' @return a vector of characters
#' @export
#' @examples
#' nucleotides <- toChars("ACTTGCTAAACTTGA")
toChars <- function(str) {
	if (!is.character(str) || length(str) != 1) {
		stop("input must be a single character string")
	}
	sapply(1:nchar(str), function(i) substr(str,i,i))
}

#' Checks if a string is a valid color
#' 
#' Given a character vector, this function will check whether each element in the vector
#' is a valid color descriptor, such as "black", "chartreuse2", or "#ff0000"
#' @param str a character vector
#' @return a logical vector of the same length indicating which elements are valid colors
#' @references \url{http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation/13290832#13290832}
#' @export
#' @examples
#' is.color(c(NA, "black", "blackk", "1", "#00", "#000000"))
is.color <- function(str) {
	sapply(str, function(x) {
		tryCatch(
			is.matrix(col2rgb(x)), 
			error = function(e) FALSE
		)
	})
}

#' Add alpha channel
#' 
#' Adds an alpha channel (i.e. transparency) to a predefined color
#' @param color a predefined color string (e.g. "firebrick")
#' @param alpha a number between 0 and 1 for the alpha channel value
#' @return the corresponding color with the added alpha channel value
#' @export
#' @examples
#' transparentChartreuse <- colAlpha("chartreuse3",0.3)
colAlpha <- function(color, alpha) {
	if (length(color) != 1) {
		stop("argument 'color' can only be a single value!")
	}
	if (length(alpha) != 1 || !is.numeric(alpha) || alpha < 0 || alpha > 1) {
		stop("alpha must be a single numerical value between 0 and 1")
	}
	if (!is.color(color)) {
		stop("argument 'color' must be a valid color!")
	}
	do.call(rgb,as.list(c(col2rgb(color)[,1],alpha=alpha*255,maxColorValue=255)))
}


#' Create a color gradient function
#' 
#' Creates a color gradient function that maps numerical values to colors on a gradient.
#' Multiple stops in the gradient can be defined as different input colors and be assigned
#' to numerical values. For example, a gradient could start at '0' with the color blue, transition
#' towards white as it approaches '1' and further transition to red as it approaches '2'. Values
#' outside of the defined range would be mapped to the nearest extreme color; e.g. in the previous
#' example, '3.1' would still map to red.
#' 
#' @param valStops a vector listing the numerical values mapped to the color stops. Defaults to
#'   \code{c(0,1,2)}. 
#' @param colStops a vector of color strings. Defaults to \code{c("royalblue3","white","firebrick3")}.
#' @param naCol the color to assign to NA values. Defaults to "gray"
#' @return a function accepting a numerical vector as input, which will produce the 
#'   corresponding color vector.
#' @export
#' @examples
#' mycolmap <- colmap(c(0,1,2),c("royalblue3","white","firebrick3"),naCol="gray")
#' mycolors <- mycolmap(seq(0,2,0.01))
colmap <- function(valStops = c(0,1,2), colStops = c("royalblue3","white","firebrick3"), naCol="gray") {
	if (!all(is.color(colStops))){
		stop("colStops must contain valid colors")
	}
	if (!is.color(naCol)) {
		stop("naCol must be a valid color")
	}
	spacing <- 1/(length(valStops)-1)
	function(vals) {
		ramp <- colorRamp(colStops)
		trvals <- sapply(vals,function(x) {
			if (is.na(x)) NA
			else if (x <= valStops[[1]]) 0
			else if (x >= valStops[[length(valStops)]]) 1
			else {
				stopIdx <- if (!any(valStops > x)) 1 else which(valStops > x)[[1]]-1
				offset <- (x-valStops[[stopIdx]])/(valStops[[stopIdx+1]]-valStops[[stopIdx]])
				spacing*(stopIdx-1) + offset*spacing
			}
		})
		apply(ramp(trvals),1,function(x) if (any(is.na(x))) naCol else do.call(rgb,as.list(x/255)))
	}
}




#' Cluster mapper
#' 
#' Constructor for an object supporting simple connected-component-clustering
#' 
#' The process starts with n objects, each in their own cluster. Whenever a link
#' is between two objects is reported, their clusters are merged.
#' Contains the following functions:
#' \itemize{
#'   \item \code{addLink(i,j)}: Creates a new link between items i and j. Whenever a link
#'      is created, the clusters encompassing the two objects are merged.
#'   \item \code{getClusters()}: Returns a list of lists representing the clusters
#'   \item \code{getIdxOf(i)}: Returns the cluster index of a given object.
#' }
#' @param n The number of elements to cluster.
#' @return the mapper object
#' @export
#' @examples
#' cmap <- new.cluster.map(10)
#' cmap$addLink(1,5)
#' cmap$addLink(3,5)
#' cmap$addLink(1,5)
#' cmap$getClusters()
new.cluster.map <- function(n) {
	
	.clusters <- as.list(1:n)

	.getIdx <- function(i) which(sapply(.clusters,function(x) i %in% x))

	addLink <- function(i,j) {
		i.idx <- .getIdx(i)
		j.idx <- .getIdx(j)
		joint <- union(.clusters[[i.idx]],.clusters[[j.idx]])
		.clusters[c(i.idx,j.idx)] <<- NULL
		.clusters[[length(.clusters)+1]] <<- joint
	}

	getClusters <- function() .clusters

	list(addLink=addLink, getClusters=getClusters, getIdxOf=.getIdx)
}


#' Topographical scatterplot
#' 
#' Draw a 'topographical scatterplot' or 'scatter-heatmap' 
#' 
#' @param x a vector of x-coordinates
#' @param y a vector of y-coordinates (must match x in length)
#' @param resolution the resolution of the plot, that is into how many bins each axis will
#'    be sub-divided. Defaults to 20.
#' @param thresh a threshold of how many data points must fall into a bin before it they 
#'    replaced with a density-indicating color
#' @param topoCol a vector of colors defining a color gradient to use for the density map
#' @param xlim the x-axis range (see plot()).
#' @param ylim the y-axis range (see plot()).
#' @param log As in plot(). "x" draws the x-axis in log scale, "y" draws the y-axis in log-scale, 
#'    "xy" draws both in log scale, "" draws both linearly. 
#' 
#' @export
#' @examples
#' x <- rnorm(10000,0,1)+10
#' y <- rnorm(10000,0,1)+10
#' topoScatter(x,y,resolution=60,xlab="foo",ylab="bar",thresh=2)
#' topoScatter(x,y,log="xy",resolution=60,xlab="foo",ylab="bar")
#'
topoScatter <- function(x,y,resolution=20,thresh=5,
					topoCol=colorRampPalette(c("black","red","yellow"))(20), maxFreq=NULL,
					xlim=range(x,na.rm=TRUE),ylim=range(y,na.rm=TRUE),log="",...) {

	if (length(x) != length(y)) {
		stop("x and y must be of same length!")
	}

	#remove infinite values and na
	fin2 <- function(x) x[apply(x,1,function(.x) all(!is.na(.x) & is.finite(.x))),]
	xy <- cbind(x,y)
	xy <- fin2(xy)
	x <- xy[,1]
	y <- xy[,2]

	widenRange <- function(x) {
		a <- x[[1]]
		b <- x[[2]]
		.a <- a - (b-a)/10000
		.b <- b + (b-a)/10000
		if (a > 0 && .a < 0) .a <- a/10
		return(c(.a,.b))
	}
	xlim <- widenRange(xlim)
	ylim <- widenRange(ylim)

	xBins <- if (regexpr("x",log) > 0) {
		exp(log(10)*seq(log10(xlim[[1]]),log10(xlim[[2]]),length.out=resolution))
	} else {
		seq(xlim[[1]],xlim[[2]],length.out=resolution)
	}
	yBins <- if (regexpr("y",log) > 0) {
		exp(log(10)*seq(log10(ylim[[1]]),log10(ylim[[2]]),length.out=resolution))
	} else {
		seq(ylim[[1]],ylim[[2]],length.out=resolution)
	}
	bins <- matrix(0,ncol=length(xBins)-1,nrow=length(yBins)-1)

	for (i in 1:length(x)) {
		xbi <- max(which(xBins <= x[[i]]))
		ybi <- max(which(yBins <= y[[i]]))
		bins[[ybi,xbi]] <- bins[[ybi,xbi]]+1
	}

	restFilter <- sapply(1:length(x),function(i) {
		xbi <- max(which(xBins <= x[[i]]))
		ybi <- max(which(yBins <= y[[i]]))
		bins[[ybi,xbi]] <= thresh
	})
	xRest <- x[restFilter]
	yRest <- y[restFilter]

	maxFreq <- if (is.null(maxFreq)) max(bins) else {
		if (maxFreq < max(bins)) max(bins) else maxFreq
	}
	cat("maxFreq =",maxFreq,"\n")

	colMap <- apply(bins,c(1,2),function(v) {
		ci <- round((v/maxFreq) * (length(topoCol)-1) + 1)
		topoCol[[ci]]
	})

	plot(NULL,type="n",xlim=xlim,ylim=ylim,log=log,...)

	points(xRest,yRest,pch=20)
	for (xbi in 1:ncol(bins)) {
		for (ybi in 1:nrow(bins)) {
			if (bins[ybi,xbi] > thresh) {
				rect(xBins[[xbi]],yBins[[ybi]],xBins[[xbi+1]],yBins[[ybi+1]],col=colMap[ybi,xbi],border=NA)
			}
		}
	}

}



#' Generic function for running means/medians etc across 2D data
#'
#' @param x the axis over which bins will be formed
#' @param y the dimension on which the function 'fun' is applied
#' @param width the width of the bins
#' @param nbins The number of bins to use
#' @param fun the function that will be applied to the y values in each bin
#' @param logScale whether or not bin-size will be at log scale across x
#' @return a matrix with two columns: bin centroid, and value of fun
#' @export
runningFunction <- function(x,y,nbins,fun=mean,logScale=FALSE) {
	if (length(x) != length(y)) {
		stop("x and y must be of equal length!")
	}
	if (!is.numeric(x) || !is.numeric(y)) {
		stop("x and y must be numeric!")
	}
	if (logScale) {
		if (any(x <= 0)) {
			warning("Ignoring infinite values and values <= 0 on x-axis!")
			bad <- x <= 0 | is.infinite(x)
			x <- x[!bad]
			y <- y[!bad]
		}
		x <- log10(x)
		# width <- log10(width)
	}
	rng <- range(x,na.rm=TRUE,finite=TRUE)
	binSides <- seq(rng[[1]],rng[[2]],length.out=nbins)
	binMids <- binSides[-1]-(rng[[2]]-rng[[1]])/(2*nbins)
	out <- sapply(1:length(binMids), function(i) {
		bin <- y[which(x > binSides[[i]] & x <= binSides[[i+1]])]
		fun(bin)
	})
	if (logScale) {
		binMids <- 10^binMids
	}
	cbind(binMids,out)
}


#' Helper function for drawing p-values over barplots
#'
#' @param p p-value
#' @param i x-coordinate of the left bar 
#' @param j x-coordinate of the right bar 
#' @param h height (y-coordinate) at which to draw the bracket
#' @param s spacer size (distance between brackets; size of feet)
#' @param th tick height (how tall the tick marks will be drawn)
#' @return nothing
#' @export
drawPvalBracket <- function(p,i,j,h=1.1,s=0.02,th=0.02) {
	#choose the correct notation and build an expression object accordingly
	pExpr <- if (p < 0.001) {
		#if p=0, then the p-value is smaller than the numerical
		#  precision of the test, so we can only indicate that it's smaller than 2.2e-16
		if (p < 2.2e-16) {
			#build math expression
			expression(p < 2.2%*%10^-16)
		} else {
			#use scientific notation for values < 0.001
			#first, extract the exponent
			expo <- floor(log10(p))
			#then extract the mantissa
			sfd <- signif(p*10^-expo,digits=3)
			#use bquote to interpolate mantissa and exponent into math expression
			bquote(p == .(sfd)%*%10^.(expo))
		}
	} else {
		#use regular decimal noation for values > 0.001
		sprintf("p = %.03f",p)
	}
	#draw the bracket
	lines(c(i+s,i+s,j-s,j-s),c(h-th,h,h,h-th))
	#draw the p-value expression
	text(mean(c(i,j)),h,pExpr,pos=3,cex=0.7)
	#return nothing
	return(invisible(NULL))
}

#' Helper function for drawing error bars on barplots
#' 
#' @param xs vector of x coordinates of bars
#' @param val the values (heights) of the bars
#' @param err the size amount of error associated with each bar
#' @param l the length of the terminators of the bar
#' @param ... any other graphical parameters
#' @export
errorBars <- function(xs,val,err,l=0.01,...) {
	arrows(xs,val-err/2,xs,val+err/2,length=l,angle=90,code=3,...)
}

# #' Retrieve Protein sequence from UniProt
# #' 
# #' Retrieves the amino acid sequence for a given protein identified by
# #' a Uniprot Accession.
# #' @param The uniprot accession
# #' @return the protein sequence
# #' @export
# getUniprotSeq <- function(uniprot.acc) {

# 	url <- paste0("https://www.uniprot.org/uniprot/",uniprot.acc,".fasta")

# 	readFASTA <- function(file) {
# 		lines <- scan(file,what="character",sep="\n")
# 		if (length(lines) < 2) {
# 			stop("Invalid FASTA format in ",file)
# 		}
# 		if (substr(lines[[1]],1,1) != ">") {
# 			stop("Missing FASTA header in ",file)
# 		}
# 		paste(lines[-1],collapse="")
# 	}

# 	prot <- readFASTA(url)

# 	sapply(1:nchar(prot),function(i)substr(prot,i,i))

# }

# provean.input <- function(uniprot.acc,wt.aa) {
# 	aas <- c("A","V","L","I","M","F","Y","W","R","H","K","D","E","S","T","N","Q","G","C","P")
# 	featable <- expand.grid(pos=1:length(wt.aa),mut.aa=aas)
# 	provean.in <- data.frame(protein=uniprot.acc,pos=featable$pos,
# 		wt=wt.aa[featable$pos],mut=featable$mut.aa
# 	)
# 	write.table(provean.in,paste0(uniprot.acc,"_provean_input.txt"),
# 		sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE
# 	)
# }
