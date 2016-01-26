## These functions allow for the repeated solving of matrices to commence in reduced time
## by caching the results for each matrix solved for future requests.

## Input: A matrix
## Output: A list including the input matrix and variables that assist in caching


makeCacheMatrix <- function(x = matrix()) 
{
        solved <- NULL
        set <- function(y)
		{
                x <<- y
                solved <<- NULL
        }
        get <- function() 
		{return (x)}
        set_solved <- function(solve) 
		{solved <<- solve}
        get_solved <- function() 
		{return (solved)}
        list(set = set, get = get,
             set_solved = set_solved,
             get_solved = get_solved)
}




## Input: A list in which there is a matrix and caching assisting variables, as outputted by the ** makeCacheMatrix ** function
## Output: The solution to the matrix; if it is cached, a cached solution is used to save time

cacheSolve <- function(x, ...) 
{
        solved <- x$get_solved()
        if(!is.null(solved)) 
		{
                message("getting cached data")
                return(solved)
        }
        data <- x$get()
        solved <- solve(data, ...)
        x$set_solved(solved)
        return (solved)
}

