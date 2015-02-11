## Code includes 2 functions to cache a matrix and compute and cache
## its inverse. 

## makeCacheMatrix takes a matrix as argument and creates an object that stores 
## (caches) the matrix. Additionally it defines functions allowing retrieval and 
## assignment of cached values.

makeCacheMatrix <- function(x = matrix()) { ## argument is a matrix x
        inv <- NULL ## set the inverse of matrix to NULL
        set <- function(y = matrix()) { ## define function that assigns x a new
                x <<- y ## matrix y
                inv <<- NULL ## inverse is set to NULL
        }
        get <- function() x  ## define function that displays x
        setinv <- function(solve) inv <<- solve  ## define function that caches computed inverse
        getinv <- function() inv ## define function that retries comptued inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
} ## output a list of 4 functions


## cacheSolve function takes object (list of functions) created by makeCacheMatrix 
## as argument. If the inverse of the matrix has already been computed and cached 
## the cached inverse matrix is returned and function exited. If inverse matrix 
## has not already been computed, it is computed from the cached matrix, 
## resulting inverse matrix is cahced and returned. 


cacheSolve <- function(x, ...) {
        inv <- x$getinv() ## get cached inverse matrix (inv) if already computed
        if(!is.null(inv)) { ## if inverse has been cached pring following ...
                            ## ... message and output cached value of inv    
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## if inv has not been cached get cached matrix
        inv <- solve(data, ...) ## compute inverse of matrix
        x$setinv(inv) ## cache computed inverse of matrix (inv)
        inv  ## Return a matrix (inv) that is the inverse of 'x'
}
