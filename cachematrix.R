## makecache matrix returns the list of set,get,setmatrix and getmatrix.
## This is the function which is building cache

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
## cacheSolve's function uses makeCacheMatrix function to calculate inverse
##of matrix function. The actual solve() method is implemeted in makeCacheMatrix.
## cachesolves computes inverse through reading(get) contents from makeCacheMatric 

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
