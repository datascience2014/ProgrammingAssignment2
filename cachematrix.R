## Matrix inversion is usually a costly computation.
## These two functions are caching the inverse of a matrix 
## to avoid compute it repeatedly 

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.  It is use solve () 
## function to get the inverse of the matrix.  
## Assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  
  get<-function() x
  
  setinversion<-function(solve) m<<- solve
  
  getinversion<-function() m
 
  list(set=set, get=get,
       setinversion=setinversion,
       getinversion=getinversion)
}


## This cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix. 
## If the inversion has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inversion from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getinversion()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
   
  data <-x$get()
  m<-solve(data, ...)
  x$setinversion(m)
  m
}
