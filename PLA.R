# a line can be represented as w'x=0 where x = vector(1, x1, x2, ..)
# and w = vector(w0, w1, w2, ...) and for simplicity we set w0=1 without
# losing generality.
# the getLine function randomely generate two points p1, p2 and return the
# straight line vector w.

getLine <- function(dimen=2){
	p1 <- runif(dimen, -1, 1)
	p2 <- runif(dimen, -1, 1)
	mat <- rbind(p1,p2)
	wt <- solve(mat, matrix(1, 2, 1))
	c(1, t(wt))
}

# Generate a series of points (X, Y) given the line vector w
getLineData <- function(w){
	x = seq(-1,1,0.05)
	
	slope = - w[2]/w[3]
	intercept = - w[1]/w[3]
	y = slope*x + intercept
	cbind(x,y)
}

# get labels based on the decision line vector
getClass <- function(X, line){
	# convert to a 1*2 matrix when X is a vector (one point only)
	if(is.vector(X)) X <- matrix(X, nrow=1)	
	sign(cbind(1, X) %*% line)
}

# plot data points X, labels Y, and boundary line w
plotSim <- function(X, Y, w){
	# get the line data of g
	wline <- getLineData(w)
	plot(X[,1], X[,2], ylim=c(-1,1), xlim=c(-1,1), pch=20, col=factor(Y))
	lines(wline[,1], wline[,2], type='l', lty='dotted', col='blue')
}

# evaluate the error rate given the target line f and PLA line g
eval <- function(g, f, plot=FALSE, size=1000){
	# gen a thousand points
	X <- matrix(runif(2*size, -1, 1), size, 2)
	#print(head(X))
	Y <- getClass(X, f)
	Yp <- getClass(X, g)

	if(plot) plotSim(X, Y, g)
	sum(Y!=Yp)/size	
}

# define perceptron learning algorith, with a initial g parameter
pla <- function(trainX, trainY, g=c(0,0,0)){
	# initialize line vector
	# g <- c(0.0, 0.0, 0.0)
	# g <- getLine()
	# g <- c(1.0, 1.0, 0.0)
	tempY <- getClass(trainX, g)
	counter <- 0
	while(!all(tempY==trainY)){
		# get a series of row indices of all mislabelled points
		miss <- which(tempY!=trainY)
		# random sample from "miss"; need to deal with one-element situation
		if(length(miss)==1){
			n <- miss
		}
		else{
			n <- sample(miss, 1) # randomly select one point
		}
		x_n <- c(1, trainX[n,])
		y_n <- trainY[n]
		
		# update vector g = g + y_n*x_n
		g <- g + y_n*x_n
		tempY <- getClass(trainX, g)
		counter <- counter + 1
		if(counter>5000) break
	}
	list(g=g, counter=counter)
}

iterPLA <- function(N=10, iter){
	errSet = vector()
	countSet = vector()
	for(i in 1:iter){
		sampleX <- matrix(runif(2*N, -1, 1), N, 2)
		f <- getLine()
		sampleY <- getClass(sampleX, f)

		res <- pla(sampleX, sampleY)
		err <- eval(res[[1]], f)
		errSet <- c(errSet, err)
		countSet <- c(countSet, res[[2]])
	}
	print(mean(errSet))
	print(mean(countSet))
	list(errSet, countSet)
}
