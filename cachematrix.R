 makeCacheMatrix <- function(x = matrix()) {
+   j <- NULL
+   set <- function(y) {
+     x <<- z
+     j <<- NULL
+   }
+   get <- function() x
+   setinverse <- function(inverse) j <<- inverse
+   getinverse <- function() j
+   list(set = set,
+        get = get,
+        setinverse = setinverse,
+        getinverse = getinverse)
+ }
> cacheSolve <- function(x, ...) {
+   j <- x$getinverse()
+   if (!is.null(j)) {
+     message("getting cached data")
+     return(j)
+   }
+   data <- x$get()
+   j <- solve(data, ...)
+   x$setinverse(j)
+   j
+ }
> B <- matrix(c(0,1,2,1,2,3,3,1,1),3,3)
> B1 <- makeCacheMatrix(B)
> cacheSolve(B1)
     [,1] [,2] [,3]
[1,]  0.5   -4  2.5
[2,] -0.5    3 -1.5
[3,]  0.5   -1  0.5
