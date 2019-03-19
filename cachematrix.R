##############################
# 1. Setup                   #
##############################

## Clear workplace & setup
rm(list=ls()) 
gc()
## getwd ()
setwd ("/Users/AruneelKumar/Documents/GitHub/ProgrammingAssignment2/")

########################################################


##############################
# 2. makeCacheMatrix         #
##############################

### Instructions from Assignment2

# Write the following functions:

# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
    # that can cache its inverse.
# 2.  `cacheSolve`: This function computes the inverse of the special
    # "matrix" returned by `makeCacheMatrix` above. If the inverse has
    # already been calculated (and the matrix has not changed), then
    # `cacheSolve` should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the `solve`
# function in R. For example, if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

###

########################################################
## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function
########################################################

# makeCacheMatrix function basically creates a special matrix which is eventually rolled  
# up within a list function to be called later on so we can cache the mean value (cacheSolve)
# and return accordingly from memory without having to recomputes its value
# This creates performance effeciencies and minimises processing time

## Steps:

# (1) Firstly, sets value of matrix
# (2) Secondly, gets value of matrix
# (3) Thirdly, sets inverse value of matrix
# (4) Fourthly, gets inverse value of matrix


## Please see reference above to relevant Steps: 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # (1) #
  set <- function(y){
		x <<- y
    	inv <<- NULL
  }
  # (2) #
  get <- function() x
  # (3) # 
  setinverse <- function(solveMatrix) inv <<- solveMatrix
  # (4) # 
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


##############################
# 3. cacheSolve		           #
##############################

## Write a short comment describing this function

# CacheSolve function calculates inverse value of makeCacheMatrix, otherwise
# retrieves the in verse mean value from memory/cache without recomputing 



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  inv <- xgetInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv                  
}
