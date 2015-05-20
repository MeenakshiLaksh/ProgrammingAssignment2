## Matrix inversion is usually a costly computation
## The pair of functions below cache the inverse of a matrix
## and helps avoid repeatedly computing the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                   ## Initializing inverse to null within this function   
    set <- function(y){
      x<<- y                                      ## Replacing the orinigal matrix with the new one
      inv <<-NULL                                 ## Initializing the original matrix's mean ( as the matrix has now changed)
    }   
    get <- function() x                           ## Return the matrix
    setinverse <- function(solve) inv<<-solve     ## Set the inverse of the matrix
    getinverse <- function() inv                  ## Get the inverse of the matrix
    list (set=set, 
          get=get, 
          setinverse=setinverse,
          getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()                         ## Get cached inverse for matrix
    if(!is.null(inv)){                            ## If there was a cached inverse the function with return the cached value 
        message ("Getting the cached inverse")    ## and it will end at the return statement
        return(inv)                                                                                  
    }
    data <- x$get()                               ## If there was no cached inverse, get the matrix stored in x
    inv<-solve(data)                              ## Caculate the inverse
    x$setinverse(inv)                             ## Cache the inverse 
    inv                                           ## Return the inverse
}
