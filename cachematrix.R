## The following functions calculate the inverse of a matrix. The functions first check if the inverse has been previously calculated
## In that case, it gets the inverse of the matrix from the cache data and avoids doing the calculations again
## By the contrary, it calculates the inverse of the matrix and saves that value in cache

##chinrichsen: Initialization of x and m objects, considering x as matrix class
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## The set function is defined. The function eliminates any value of the cached inverse matrix by previous execution of cachesolve funtcion 
        set <- function(y) {
                ## Assign the input argument to x object
                x <<- y
                ## Assign the NULL value to m object
                m <<- NULL
        }
        ## The get function defines the "getter" of the object x (matrix). To get the value of the matrix  
        get <- function() x
        ## The setinverse function sets the inverse
        setinverse <- function(solve) m <<- solve
        ## This function gets the inverse
        getinverse <- function() m
        ## Finally, we have to create a list where each element has its name. This allows us to extract information using the $ operator
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is required to get the inverse of the matrix of the makeCacheMatrix function

cachesolve <- function(x, ...) {
        ## First, the function wants to retrieve the inverse of the argument x 
        m <- x$getinverse()
        ## Then, checks if the value is or is not NULL. If it's not NULL, a message showed that the value was cached
        ## in a previous calculation, and returns the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise, a message is not showed, and the function get the matrix, then calculates the inverse, set the inverse,
        ## and finally the function returns the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
