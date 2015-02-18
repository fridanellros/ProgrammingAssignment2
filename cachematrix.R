## This is a collection of functions designed to create matrices with the
## property that they cache their inverse once calculated. If the encapsulated
## matrix data is changed, the cache will be cleared.

## Function to create a cachematrix from an input matrix x.
## Note that it is assumed that x always is invertible.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         #initiate cached inverse
    
    #(re)set encapsulated matrix data
    set <- function( y ){
        x <<- y         #update encapsulated matrix
        inv <<- NULL    #reset cache
    }
    
    #return encapsulated matrix data
    get <- function(){ x }
    
    #set inverse
    setinv <- function(inverse){ inv <<- inverse }
    
    #get inverse
    getinv <- function(){ inv }
    
    #return list of functions 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to retrive a matrix inverse of the input cachematrix x. 
## Note that it is assumed that x always is invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    #inverse already cached 
    if( !is.null(inv) ){
        message("getting cached inverse")
        return(inv)
    }
    
    #inverse needs to be (re)computed
    mx <- x$get()
    inv <- solve(mx)
    x$setinv( inv )
    inv
}
