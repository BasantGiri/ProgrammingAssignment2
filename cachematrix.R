## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
inv_x <- NULL
+     set <- function(y) {
# use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
+         x <<- y
+         inv_x <<- NULL
+     }
+     get <- function() x
+     setinverse<- function(inverse) inv_x <<-inverse
+     getinverse <- function() inv_x
+     list(set = set, get = get,
+          setinverse = setinverse,
+          getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        inv_x <- x$getinverse()
        # if the inverse has already been calculated
+     if (!is.null(inv_x)) {
+         message("getting cached inverse matrix")
+         return(inv_x)
+     } else {
+         inv_x <- solve(x$get())
+         x$setinverse(inv_x)
+         return(inv_x)
+     }
}
