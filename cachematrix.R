## R File writen by Javier Saravia
## this R File contains two functions; one creates a cache matrix
## and the others gets the inverse of that matrix
## at the end of the files appears comments with a running example

## the MAKECACHEMATRIX function receives an matrix that will be stored into the cache memory
## this program need that a variable reciebe the value of the makecache function
## this variable will reciebe the set, get, setInv and getInv attributes

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## the cacheSolve function return the inverse of the matrix by the getInv attribute created at the first function

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data") ## is a matrix already exist, return this message
    return(inv)
  }
  data <- x$get() ## get a matrix
  inv <- solve(data, ...) ## the solve function is used to return the inverse of a matrix
  x$setInv(inv) ## asign the value of inverse matrix to $setInv attribute
  inv ## return the inverse of the matrix INV
}


## EXAMPLE OF FUNCTION EXECUTION!!!

## creation of matrix
## m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE) 

## print of m is:  
## [,1] [,2]
## [1,]    0    2
## [2,]    1    0

## next step is send X matrix as a parameter to the makeCacheMatrix function
## for this, I will use a new variable called mm
## mm <- makeCacheMatrix(x)

## the next step is send mm variable as a parameter to cacheSolve function
## for this I will use a new variable called mmm 
### mmm will get and store the inverse of the initial matrix
## mmm <- cacheSolve(mm)

## printing mmm will show the inverse of initial matrix
## mmm

## the print of mmm is
## [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0


