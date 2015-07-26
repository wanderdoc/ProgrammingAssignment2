makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL                     # undefine the value of the matrix inverse
  
  set <- function(y)              # set the matrix
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x             # get the matrix
  
  
  setinverse <- function(inverse) # set the value of the matrix inverse
  {
    inv <<- inverse
  }
  
  getinverse <- function() inv    # get the value of the matrix inverse
  
  # Return value is a list of functions to
  # set the matrix
  # get the matrix
  # set the value of the matrix inverse
  # get the value of the matrix inverse:
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) 
{
  # Is there a value there?
  inv <- x$getinverse()
  # If yes, take this value and do not calculate the inverse.
  if(!is.null(inv)) 
  {
    message("getting cached data.")
    return(inv)
  }
  # Else: take the matrix.
  data <- x$get()
  # Sovle it.
  inv <- solve(data)
  # Save the inverse value of it.
  x$setinverse(inv)
  # Return this value.
  inv
}

# Some tests:
x<-matrix(c(1, -.5, -.5, 1), ncol=2)
mx<-makeCacheMatrix(x)
cacheSolve(mx)
cacheSolve(mx) # prints "getting cached data".
y<-matrix(c(2, -2, -.1, 1), ncol=2)
my<-makeCacheMatrix(y)
cacheSolve(my)
cacheSolve(my) # prints "getting cached data".
