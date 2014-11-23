## Calculates the inverse of a matrix and save it in a cache which can be reused in 
## further calculations. If the inverse has been calculated then retrieve the result
## from the cache

## A function that creates a special vector, i.e. a list containing functions that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix (allowing it to be cached)
## 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- null
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    
    ## get the inverse of matrix
    getinverse <- function () m
    
    ## build the list of object
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x' by using the makeCacheMatrix function
## If the inverse have been calculated then this function will return the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## get the inverse of the matrix
        m <- x$getinverse()
        
        ## check if there is a matrix in the cache
        ## if it's in the cache then return the cached matrix
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        
        ## if there is nothing in the cache then calculate the inverse,
        data <- x$get()
        m <- solve(data, ...)
        
        ## save the inverse in the cache
        x$setinverse(m)
        m
        
        
        
}
