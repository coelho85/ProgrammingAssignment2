## The set of two functions below described are pretty much
## similar to the given example with vectors.
## Basically, the function makeCacheMatrix creates a matrix
## inserted in the line command by the user and sets its inverse to NULL.
## The function cacheSolve calculates the inverse of the matrix previously
## created and sets its inverse into the m variable, which will be tested
## everytime the function is called, avoiding double calculation.


## Write a short comment describing this function
## As before mentioned, this function creates a matrix
## e.g:  a <- makeCacheMatrix(matrix(c(4,3,3,2),2,2)) or a$set(matrix(c(4,3,3,2),2,2))
## It is important to mention that m is set to NULL, making its inverse 
## equal to NULL everytime a new matrix is loaded.

makeCacheMatrix <- function(x = matrix()) {
	
	m<- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	 get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse= getinverse)
	
}


## Write a short comment describing this function
## This function uses the matrix created in the previous function as input
## and calculates its inverse by using the R function "solve()".
## The output is stored in the variable "m". So, everytime the function
## is called again, it first tests whether the inverse was previously
## calculated and, instead of calculating the inverse again, it returns
## the variable m, avoinding the function "solve()".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m

}
