## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## This script includes two functions, makeCacheMatrix and cacheSolve that cache 
## the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invM<-NULL
    #set the matrix
    set<-function(y){
        x<<-y
        invM<<-NULL
    }
    #get the matrix
    get<-function() x
    #set the inverse of the matrix
    setInv<-function(inv) invM<<-inv
    #get the inverse of the matrix
    getInv<-function() invM
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## This function calcultes the mean of the "matrix" created by the makeCacheMatrix 
## function.

cacheSolve <- function(x, ...) {
    invM<-x$getInv()
    #If inverse already calculated, return cached value
    if(!is.null(invM)){
        message("getting cached matrix inverse")
        return(invM)
    }
    #If not, calculate matrix inverse and cache it in x
    data<-x$get()
    invM<-solve(data)
    #cache calculated matrix inverse
    x$setInv(invM)
    invM
}
