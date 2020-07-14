## This fucntions takes an invertible matrix and has the capacity to store
## the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL       ## variable to store the inverse of the given matrix
        set <- function (y) {
                x <<-y
                s <<-NULL ## We flush any store inverse previously stored
        }
        get <- function () x
        setsolve <- function(solve) s <<-solve
        getsolve <- function() s
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve) 
}


## This functions calculates the inverse (solve) of a given invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) { ## If the inverse is already cached, don't solve it again
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
