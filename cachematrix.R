## create functions to calculate inversion of matrix.
## inverted matrix will be cached , so in case inverted matrix need to be calculated again it will be fetched from cache
## this will increase performance in case of big matrixs

## Creates matrix and return list of following functions
   ## set: to set the value of matrix
   ## get: to get the value of matrix
   ## setInverseMatrix: set the inverted matrix
   ## getInverseMatrix: get the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) invertedMatrix <<- solve
  getInverseMatrix <- function() invertedMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## Write a short comment describing this function
## Calculate the invertion of matrix created by makeCacheMatrix function
## cacheSolve first check if inverted matrix has already been calculated, if yes it reutrns the inverted matrix 
## from cache instead of computing it again , if no it compute inverted matrix and set the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getInverseMatrix()
        if(!is.null(invertedMatrix)) {
          message("getting cached data")
          return(invertedMatrix)
        }
        data <- x$get()
        invertedMatrix <- solve(data, ...)
        x$setInverseMatrix(invertedMatrix)
        invertedMatrix
}
