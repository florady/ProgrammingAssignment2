## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object, which can cache its inverse
##### 1. set the value of the  matrix;          2. get the value of the matrix;
##### 3. calculate the inverse of the matrix;   4. get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}

##### Function cacheSolve computes the inverse of the special "matrix" returned 
##### by makeCacheMatrix above.
##### If the inverse has already been calculated (and the matrix has not changed),
##### then the cacheSolve should retrieve the inverse from the cache.
##### Otherwise, it calculates the inverse of the matrix and sets the inverse 
##### of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data, ...)
      x$setmatrix(m)
      m
}
