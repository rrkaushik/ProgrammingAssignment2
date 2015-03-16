
## function to "proxy" for a matrix operation.
## returns a list with set, get and setinverse/getinverse function "constructors"
## allows for the cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the 'mean' variable to NULL; must
    # do so that the function doesn't hang the
    # first time through.
    m <- NULL
    
    set <- function(y) { # 'y' is the args passed into 'makeCacheMatrix'
        x <<- y # Set 'x' for the function environment to 'y'
        m <<- NULL # Set 'm' for the 'makeCacheMatrix' environment to NULL
    }
    get <- function() x # Create a function 'get' in the 'makeCacheMatrix' parent and assigns a matrix to it
    
    setinverse <- function(inverse) m <<- inverse  # Takes a value ('inverse') and sets it to the value of 'm' in the 'makeCacheMatrix' frame.
    
    getinverse <- function() m # returns the value of 'm' from the 'makeCacheMatrix' frame.
    
    #Lists out the values of the functions in the makeCacheMatrix frame
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
    # returns a list of set, get, setInv, getInv which can be used as a proxy
}


## function to invert a matrix. Either returns a previously cached copy of the inverted matrix
## or inverts it and sets the cached version to the calculated value

cacheSolve <- function(x, ...) {

    m <- x$getinverse() # Goes to the'x' environment and assigns the 'm' value from that environment to this one.
    
    # If the 'x' environment has been evaluated 
    # before, the function prints the message and
    # the value of m (the cached inverse).
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    
    data <- x$get() # If this particular 'x' has never been
                    # evaluated before, pull the x-vector into a
                    # local varaible called 'data'.
    
    m <- solve(data, ...) # Calculate the inverse of the matrix x by calling
                            # 'solve' function on the data local variable.
    
    x$setinverse(m) # Assign the calculated miverse to the 'x' environment using the 'setinverse' function
    
    m    # Return the calculated inverse
}

### CALLING DESIGN PATTERN ###
#v <- 1:100 # the variables you want to operate on - vector, matrix etc.
#vc <- makeVector() # construct the intermediate proxy for "make" constructors
#vc$set(v) #pass your variable to set OPTIONAL
#cachemean(vc) # call function using the proxy


# # TESTING CODE ##
# rm(list=ls())
# source("cachematrix.R")
# 
# m <- matrix(c(-1, -2, 1, 1), 2, 2)
# m_proxy <- makeCacheMatrix(m)
# # mc$set(m_proxy) -- OPTIONAL
# inv <- cacheSolve(m_proxy)
# # Multiply matrices to get identity matrix
# m %*% inv 

# ORIGINAL CODE ##
# makeVector <- function(x = numeric()) {
#     m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }






