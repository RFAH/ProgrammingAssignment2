> ## This function creates a special "matrix" object that can
> ## cache its inverse. Specifically it will
> ## 1. set the value of the matrix
> ## 2. get the value of the matrix
> ## 3. set the value of the inverse matrix
> ## 4. get the value of the inverse matrix
> 
> makeCacheMatrix <- function(x = matrix()) {
+ inv <- NULL
+ set <- function(y) {
+ x <<- y
+ inv <<- NULL
+ }
+ get <- function() x
+ setinverse <- function(solve) inv <<- solve
+ getinverse <- function() inv
+ list(set = set, get = get, setinverse = setinverse, 
+ getinverse = getinverse)
+ }
> 
> 
> ## This function calculates the inverse of the special "matrix" 
> ## returned by the function makeCacheMatrix above. If the 
> ## inverse has already been calculated (and the matrix has not
> ## changed), then the cachesolve should return the inverse from 
> ## the cache. 
> 
> cacheSolve <- function(x, ...) {
+       inv <- x$getinverse()
+ if(!is.null(inv)) {
+ message("getting cached data")
+ return(inv)
+ }
+ data <- x$get()
+ inv <- solve(data, ...)
+ x$setinverse(inv)
+ inv
+ }

