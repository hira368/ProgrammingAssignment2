


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
sample_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
> special_matrix <- makeCacheMatrix(sample_matrix)
> inverse_matrix <- cacheSolve(special_matrix)
> print("Inverse of the matrix:")
> print(inverse_matrix)
> cached_inverse_matrix <- cacheSolve(special_matrix)
> print("Cached inverse of the matrix:")
> print(cached_inverse_matrix)

