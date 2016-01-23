## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Before executing the cacheSolve function, create a square matrix with, for example:
## > a<-makeCacheMatrix()
## > a$set(matrix(runif(16), 4, 4)


makeCacheMatrix <- function(x = matrix()) {
        cache<-NULL                             ## making sure nothing is stored in Cache
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve   ## first time inverse the matrix and store to cache
        getmatrix<-function() m                 
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()                        ## fetching the inverse matrix from cache
        if(!is.null(m)){                        ## if the m is stored, return the inverse matrix from cache
                message("data from cache")
                return(m)
        }
        matrix<-x$get()                         ## otherwise inverse matrix manually
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
