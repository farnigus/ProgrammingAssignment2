## Functions to create a matrix-like object which can cache its own inverse


## makeCacheMatrix defines two variables and four functions. 

## x is the data matrix 

## inv is the inverse of x. Initialized to null on creation of object 
## or when reassigning X


makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function()inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse of a cacheMatrix
## if the inverse is previously solved, then simply return the value

## if inverse has not yet been calculated, then calculate the inverse
## and cache the result

cacheSolve <- function(x, ...) {
        
    inv<-x$getinv()
    if(!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    message("No cached value found. Calculating inverse. ")
    data<-x$get()
    inv<-solve(data)
    x$setinv(inv)
    inv
}
