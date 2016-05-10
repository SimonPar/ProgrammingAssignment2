## a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ## use '<<-' to assign a value to an object in an 
                ## environment different than the current one.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        inv <- x$getinv
        # if the inverse has already been calculated
        if (!is.null(inv)) {
                #get it from cache and skip computation.
                message("getting cached data")
                return(inv)
        }
        # if data not existent, calculate the inverse
        matrix.data <- x$get()
        inv<- solve(matric.data, ...)
        #caches the value of the inverse through the setinv function
        x$setinv(inv)
        return(inv)
}
