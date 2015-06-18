## This file contains a pair of functions that compute and cache the inversion of a certain matrix. 
## With these functions the users can compute the inversion of a matrix input , storge and recall the results
## and rewrite the computation repeatly.

## The first function as follows contains four sub-function:
## 1. set(): set or reset the matrix for computation
## 2. get(): get the matrix for computation
## 3. setinverse(): set the inversion of the matrix
## 4. getinverse(): get the result of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
         m<-NULL
         set<-function(y){
         x<<-y
          m<<-NULL
}
         get<-function()x
          setinverse<-function(inverse) m<<-inverse
          getinverse<-function()m
          list(set=set,get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}


## The following function caculate the inverse matrix and set it in the cache via setinverse() function. 
## At first it will checked if the inversion has already been calculated, and if so, it will skip the calculation and
## recall the result from the cache.

cacheSolve <- function(x, ...) {
           m<-x$getinverse()
           if(!is.null(m)){
             message("getting cached data")
             return(m)
           }
           data<-x$get()
        ## Return a matrix that is the inverse of 'x'
           m<-solve(data)
           x$setinverse(m)
           m
}
