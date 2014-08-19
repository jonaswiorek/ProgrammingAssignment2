## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## The "matrix" is assumed to be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        invMtx <- NULL
        set <- function(y){
                x <<- y
                invMtx <<- NULL
        }
        get <- function() {
                x
        }
        setInverseMatrix <- function(invMatrix) {
                invMtx <<- invMatrix 
        }
        getInverseMatrix <- function() {
                invMtx
        }
        list(set = set, get = get, 
             setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the function should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtx <- x$getInverseMatrix()
        if(!is.null(invMtx)) {
                message('getting cashed inverse of matrix')
                return(invMtx)
        }
        mtx <- x$get()
        invMtx <- solve(mtx,...) ## Assumes a square invertible matrix
        x$setInverseMatrix(invMtx)
        invMtx
}
