## This function creates a special "matrix" object that can cache its inverse.
## 4 different subfunctions are defined: 
#--- get and set for the regular matrix
#--- getInverse and setInverse for the inverse matrix
#get returns the input matrix
#set updates the input matrix and resets the cached inverse to null
#getInverse returns the cached inverse (if null, returns null)
#setInverse updates the cached inverse
##Can be tested using matrix: a <- matrix(c(1,2,3,4,5,6,7,8,1), nrow = 3, ncol = 3)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix){
    x <<- matrix
    inverse <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverseMat){ inverse <<- inverseMat}
  getInverse <- function(){inverse}
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Function cacheSolve
## Return a matrix that is the inverse of 'x', if possible from the cache
#input: matrix
#output: inverse of the input matrix
#--First an attempt is made to retrieve the inverse from the cache
#--If this succeeded, a message is printed and the inverse is simply returned
#--If no cached inverse can be found, the actual matrix is retrieved and the inverse is calculated
#--For future usage, the inverse is saved in the cache and returned as result of the function


cacheSolve <- function(x) {
  inverse <- x$getInverse()
  if (!is.null(inverse)){
    message ("Getting the cached inverse")
    return(inverse)
  }
  matrix <- x$get()
  message ("Calculating the inverse")
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
