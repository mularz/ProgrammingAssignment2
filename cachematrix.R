## These two functions - makeCacheMatrix and cacheSolve - take advantage of 
## the scoping rules of R and the ability to treat a function as a first class 
## object. The cacheSolve function use the functions defined in makeCacheMatrix 
## to solve for the inverse of a matrix originally supplied to makeCacheMatrix. 
## 

#############################################################################
##makeCacheMatrix
##  v1.0 / May 2014
##
##  input:  
##      x - a square matrix (equal rows and columns) is assumed
##  output: 
##      list of methods to call on the matrix x
## description:
##   makeCacheMatrix,takes as input a square matrix (x),and implements several
##   functions for setting and getting the matrix and its inverse that are
##   output as a list. These functions can then be called by other functions
##   for setting/getting the matrix and its inverse.
##   get        - retrieves the matrix x supplied to makeCacheMatrix
##   set        - saves the matrix x and initializes inverse to NULL, 
##                  indicating that no inverse has been computed or cached
##   getInverse - retrieves the cached version of inverse
##   setInverse - stores an inverse for the current matrix by 
##                  executing a function that assigns the output of solve 
##                  to inverse, thereby caching the result.
##   

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-function(y) {
        x<<-y
        inverse<<- NULL
    }
    get <- function() x
    setInverse<- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list (set = set, 
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


#############################################################################
##cacheSolve
##  v1.0 / May 2014
##
##  input:  
##      output of makeCacheMatrix (list of methods)
##  output: 
##      inverse of matrix orginally supplied to makeCacheMatrix (inverse)
## description:
##       Retrieves the value of inverse using the getInverse function 
##       in makeCacheMatrix
##      If inverse is NOT null,
##       it has retrieved the cached version and no computation required
##      If it is null, 
##          this function retrieves the original matrix (x$get()), 
##          solves for the inverse and saves the results in 
##          inverse by executing x$setInverse(inverse).  The next
##          call to cacheSolve will find a cached result.
##   
############################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}
