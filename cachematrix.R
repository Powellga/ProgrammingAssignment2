##Gregg A Powell
##g.a.powell@protonmail.com
#15MAY2020

## Put comments here that give an overall description of what your functions do........................


### makeCacheMatrix creates a "matrix" or a list containing a function that:
### (1)sets the value of the matrix, (2) gets the value of the matrix
### (3)sets the value of the inverse, and (4) gets the value of the inverse

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
  set.it <- function(n) {   
    m <<- n
    i <<- NULL
    ##(1)sets the value of the matrix
  }
    get.it <- function() m
    ##(2) gets the value of the matrix
  
    set.inverse <- function(inverse) i <<- inverse
  
    get.inverse <- function() i
    list(set.it = set.it, get.it = get.it, set.inverse = set.inverse, get.inverse = get.inverse)
     ##(3)sets the value of the inverse
     ##(4) gets the value of the inverse
}


###  The cachesolve function below computes the inverse of the "matrix" returned by makeCacheMatrix defined in the 
###  previous function above. If the inverse of the matrix was previously calculated and didn't change, 
###  then the cacheSolve function below performs an inverse matrix calculation on the matrix stored in the cache.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  j <- m$get.inverse()
  
  
  if (!is.null(j)) {
    message("**    pulling cached MATRIX -  Morpheus...HELP!   ;)    **")
    return(j)
  }
  
  data <- m$get.it()
  j <- solve(data, ...)
  m$set.inverse(j)
  j
  
}


######################END############################################

######################Check Work#####################################


Example <- matrix(c(1,2,3,4,5,6,7,8,0),3,3)

CacheExample <- makeCacheMatrix(Example)

Example  #displays example matrix
CacheExample 

cacheSolve(CacheExample)   #returns matrix inverse  x2
cacheSolve(CacheExample)   #returns matrix inverse
