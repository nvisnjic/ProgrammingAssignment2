## RProg-010 assignment 2
## Doing a bit of magic to construct a container for matrices
## which can be used to recall previously computed inverted matrices
## Pros: Less computing required! 
## Cons: More RAM used for each "special" matrix container


## Matrix contrainer constructor with functions calls
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverz <- function(solve) m <<- solve
    getinverz <- function() m
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
    
    m <- x$getinverz()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverz(m)
    m

}
