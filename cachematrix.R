## the following function creates a list of 4 components
## Each component is a function (set, get, setInverse and getInverse)

makeCacheMatrix <- function(x = matrix()) 
{
  inverse = NULL
  set = function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  get = function() x
  
  setInverse = function(inverse) inverse <<- inverse
  
  getInverse = function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# The following function calcuates the inverse of the matrix if it is not in the
# cache yet. It will skip the calculation if the inverse is already in the cache
# Function solve is used to compute the inverse

cacheSolve <- function(x, ...) 
{
   
  inverse = x$getInverse()
  
  if(!is.null(inverse))
  {
    # Indicating cached data is being retrieved
    message("Getting cached data")
    return(inverse)
  }

  data = x$get()
  
  inverse = solve(data, ...)
  x$setInverse(inverse)
  inverse

}
