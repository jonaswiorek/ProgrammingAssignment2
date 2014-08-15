## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y){
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() {
                x
        }
        setInverseMatrix <- function(invM) {
                inverseMatrix <<- invM 
        }
        getInverseMatrix <- function() {
                inverseMatrix
        }
        list(set = set, get = get, 
             setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtx <- x$getInverseMatrix()
        if(!is.null(invMtx)) {
                message('getting cashed inverse of matrix')
                return(invMtx)
        }
        mtx <- x$get()
        invMtx <- solve(mtx,...)
        x$setInverseMatrix(invMtx)
        invMtx
}
