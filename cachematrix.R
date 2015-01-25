## RProg-010 assignment 2
## Doing a bit of magic to construct a container for matrices
## which can be used to recall previously computed inverted matrices
## Pros: Less computing required! (if we inverse same matrix containter)
## Cons: More RAM used for each "special" matrix container


## Matrix contrainer constructor with functions calls for manipulation
makeCacheMatrix <- function(x = matrix()) {
    
    # Clear inverted matrix m
    m <- NULL
    set <- function(y) {
        # a bit of scoping in action
        # using CacheMatrix$set on object we can change value of cached matrix
        # inverted matrix m gets cleared anyway
        x <<- y
        m <<- NULL
    }
    # functions for manipulating cache matrix object
    # fetch me the cached matrix
    get <- function() x
    # set the value of the computed inverse
    setinverz <- function(solve) m <<- solve
    # fetch me the inverted matrix
    getinverz <- function() m
    # container list we return from the constructor  
    list(set = set, get = get,
         setinverz = setinverz,
         getinverz = getinverz)
}


## Solver function with logic for checking if inverted matrix
## already solved for specified input matrix container
## Does not check value in other containers, only if the
## provided container is solved previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
    # call to fetch value of inverted matrix
    m <- x$getinverz()
    if(!is.null(m)) { # was the inverse already computed?
        # if yes, spill it out and exit
        message("getting cached data")
        return(m)
    }
    # So it wasn't computed, get me the matrix then
    data <- x$get()
    # compute the inverted matrix
    m <- solve(data, ...)
    # push value to m in matrix container
    x$setinverz(m)
    # return and done
    m

}
