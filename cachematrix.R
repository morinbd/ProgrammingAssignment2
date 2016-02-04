## These functions provide the ability to cache the results of time consuming
## computations so that they can be looked up later instead of computing
## them again. These examples are applied to matrix inversion which can be a
## costly computation.a

## The first function will create a speacial matrix object that can cache it's 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cachesolve will do the computation of the special matrix returned by the 
## fuction above. You can see that if the matrix has been previously been computed
## the function will retrieve the inverse from the cache. If on the otherhand
## the inverse has not been computed then it will be calculated and stored.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
