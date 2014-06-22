## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  c<-NULL
  set<-function(y){
    x<<-y
    c<<-NULL
  }
  get<-function() x
  setcache<-function(cache) {
    c<<-cache
  }
  getcache<-function() {
    c
  }
  list(set=set,get=get,setcache=setcache,getcache=getcache)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setcache function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c<-x$getcache()
  if(!is.null(c)){
    return(c)
  }
  data<-x$get()
  cc<-solve(data)
  x$setcache(cc)
  cc
}

## Use the following script to test
##source("cachematrix.R")
##m<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3, byrow=TRUE))
##m$get()
##m$getcache()  ## This should return NULL
##cacheSolve(m)
##m$getcache()  ## This should return the correct inverse matrix
