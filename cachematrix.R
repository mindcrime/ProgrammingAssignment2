## Assignment2 for the Coursera R Programming course

## a function that returns a List made to act something like an object by giving it functions
## as elements, and using the lexical scoping rules in R to access state that's "in" the
## pseudo "object" we create.

makeCacheMatrix <- function(x = matrix())
{
	inverse = NULL
	set <- function(y)
	{
		x <<- y
	       	inverse <<- NULL
	}

	get <- function() x
        setInverse <- function(newInverse) inverse <<- newInverse
	getInverse <- function() inverse

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes one of our "pseudo object" items from makeCacheMatrix and tries to retrieve
## the inverse value from the cache. If there's a value there, just return it. Otherwise,
## access the underlying matrix, calculate the inverse, and save the inverse into the
## variable that's acting as our cache.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	
        if(!is.null(inverse))
	{
	    message("using cached data")
	    return(inverse)
	}

       matrix <- x$get()
       inverse <- solve(matrix, ...)
       x$setInverse(inverse)
       inverse
}
