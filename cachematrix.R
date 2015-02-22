## makeCacheMatrix: Store and retrieve a object (matrix) in the cache.
## Params: a matrix
## Result: a special object with the functions below:
##              1- set the value of the matrix (set)
##              2- get the value of the matrix (get)
##              3- set the value of the solve (setInverse)
##              4- get the value of the solve (getInverse)
## Author: Daniel Lins
## Date:02/21/2015
## Version:1.0
makeCacheMatrix <- function(x = matrix()) {
        
        cachedInverseMatrix <- NULL
        set <- function(y){
                x <<- y
                cachedInverseMatrix <<- NULL        
        }
        get <- function() x
        setInverse <- function(inverseMatrix) cachedInverseMatrix <<- inverseMatrix 
        getInverse <- function() cachedInverseMatrix
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: Calculates the inverse (solve) of the matrix. However, it first 
##             checks if the result has already been calculated and stored in the cache. 
##             If yes, it gets the result from the cache.
## Params: Special object (output of MakeCacheMatrix())
## Return: a matrix that is the inverse of the original matrix.
## Author: Daniel Lins
## Date:02/21/2015
## Version:1.0
cacheSolve <- function(x, ...) {
        
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)){
                message("Object already in Cache. Retrieving cached data...")
                return (inverseMatrix)
        }
        
        originalMatrix <-x$get()
        inverseMatrix <- solve(originalMatrix)
        x$setInverse(inverseMatrix)
        return(inverseMatrix)
}
