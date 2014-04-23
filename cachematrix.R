## makeCacheMatrix and cacheSolve below are a pair of functions
## that allow to create a special matrix object that can cache and retrieve its inverse


## makeCacheMatrix is a fucntion that takes argument 'x' of class matrix
## and creates an object with 4 functions that can make the following operations on 'x':
## store 'x', retrieve 'x', calculate and cache inverse of 'x' and retrieve inverse of 'x' from cache 

makeCacheMatrix <- function(x = matrix()) {
                
                x.inv <- NULL #clear content of cache store for inverse of x
                
                set <- function(y) { #call function to store argument in object x
                        x <<- y
                        x.inv <<- NULL #empty cached inverse
                }
                
                get <- function() x # call function to retrive matrix from object x
  
                setinv <- function(solve) x.inv <<- solve #call function
                # to invert matrix stored in object x and cache the inverse
                
                getinv <- function() x.inv #call function to retrieve inverse from cache
                
                
                list(set = set, get = get, #return a list of functions for object makeCacheMatrix
                        setinv = setinv,
                        getinv = getinv)

}


## cacheSolve is a fucntion that caclulates and retrieves an inverse of input matrix 'x' 
## from cache

cacheSolve <- function(x, ...) {
        
        x.inv <- x$getinv() # attempt to retrieve inverse of 'x' from cache
        
        #return inverse of 'x' that has already been cached without calculations
        if(!is.null(x.inv)) { 
                message("getting cached data")
                return(x.inv)
        }
        
        #inverse of 'x' was not previously cached
        data <- x$get()
        x.inv <- solve(data, ...) #calculate inverse of 'x'
        x$setinv(x.inv) #cache inverse of 'x'
        x.inv #return inverse of 'x'
        
}
