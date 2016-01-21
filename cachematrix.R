## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x
        setInverseM <- function(NewInverseM) inverseM <<- NewInverseM
        getInverseM <- function() inverseM
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM <- x$getInverseM()
        if(!is.null(inverseM)) {
                message("getting cached `inverse Matrix")
                return(inverseM)
        }
        data <- x$get()
        inverseM <- solve(data, ...)
        x$setInverseM(inverseM)
        inverseM
}
