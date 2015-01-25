## Creates an object to cache the inverse of a matrix. Uses the object to either retrieve the inverse of any matrix using cached data
## (if the cached inverse for that matrix is already stored) or by calculating and returning the inverse of the matrix, storing this new inverse matrix in the object.

## Create an object to store the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        if (dim(y) == dim(x) && all(x==y)) {
            message("matrix has not changed")
        }
        else {
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x
    setInv <- function(inverse) i <<- inverse
    getInv <- function() i
    list(set = set, get = get,
    setInv = setInv,
    getInv = getInv)
}

## Retrieve inverse of matrix using cached data if already stored, otherwise calculate, return and store the new matrix inverse

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
        message("returning cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
