## makeCacheMatrix and cacheSolve functions are used to create a
## special object that stores a matrix and caches the matrix inverse.

## The first function, `makeCacheMatrix` creates a special object, which is
## a list containing a function to

## 1.  set the value of the matrix - setMatrix
## 2.  get the value of the matrix - getMatrix
## 3.  set the inverse of the matrix - setInverseMatrix
## 4.  get the inverse of the matrix - getInverseMatrix

makeCacheMatrix <- function(x = matrix()) {


  imtr <- NULL

  setMatrix <- function(m){
    x <<- m
    imtr <<- NULL

  }
  
  getMatrix <- function() x
  
  setInverseMatrix <- function(inv) imtr <<- inv
  
  getInverseMatrix <- function()  imtr
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setInverseMatrix`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverseMatrix()
  
  if(!is.null(m)){
    print("getting cached data")
    return(m)
  }

  data <- x$getMatrix()
  inverse <- solve(data)

  x$setInverseMatrix(inverse)
  inverse
}
