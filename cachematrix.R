
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 
        set <- function(y) {
                x <-y
                inv<<- NULL
}

get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)


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
samplematrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
> specialmatrix <- makeCacheMatrix(samplematrix)
> inversematrix <- cacheSolve(specialmatrix)
> print("Inverse of the matrix:")
> print(inversematrix)
> cached_inversematrix <- cacheSolve(specialmatrix)
> print("Cached inverse of the matrix:")
> print(cached_inversematrix)
