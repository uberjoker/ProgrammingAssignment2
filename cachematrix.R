## the function 'makeCacheMatrix' creates an object taking a matrix vector as input.
## the cacheSolve function checks if inverse of the inputted matrix exists in the cache, then returns the cache value
## Otherwise, it calculates the inverse and returns the value

## to do the above, the curr_mat function is called with a invertible matrix as input
## the get_mat function can be used to view the matrix got from curr_mat

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  #get the matrix 
  get_mat <- function () x
  
  # store the matrix 
  curr_mat  <- function(y) 
    {
        inv <<- NULL
        x <<- y
    }
  
  #------------------------------
  
  #get inverted matrix
  
  get_inv <- function () inv
  
  #-------------------------------
  
  # set inverted matrix
  set_inv <- function(inverse) inv <<- inverse
  
  list (get_mat = get_mat, curr_mat = curr_mat, get_inv = get_inv, set_inv = set_inv)
}


## This function takes an object of type 'makeCacheMatrix' as input
## first, it gets the value stored in the cache by the get_inv function
## if the value returned by get_inv is NOT NULL, it prints the cached value to the console
## if get_inv returns a NULL value, the inputted matrix is stored in the 'data' vector
## the inverse of the 'data' matrix is stored in the 'inv' vector
## the set_inv function of the makeCacheMatrix object is called and the calculated inverse is cached
## the calculated inverse is printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
  inv <- x$get_inv()
  
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }    
  
  data <- x$get_mat()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}



