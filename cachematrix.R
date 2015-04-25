
                                            ## These functions are able to cache the inverse matrix of any given matrix THAT IS INVERTIBLE.

                                            #makeCacheMatrix creates a list containing four functions
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }                                         #first function, used to set the value of the matrix
  get <- function() x                       #second function, used to get the value of the martix
  setMatrix <- function(solve) m <<- solve  #third function, used to set the inverse matrix
  getMatrix <- function() m                 #fourth function, used to get the value of the inverse matrix
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


                                            ## This function calculates the inverse Matrix.

cacheSolve <- function(x,...) {
        
  m <- x$getMatrix()                         
    if(!is.null(m)) {
    message("saving time-consuming computations by getting from the cache the value of the inverse Matrix")
    return(m)
  }                                         #This condition checks if the inverse matrix has already been calculated. If so, it gets the value of the cache and skips calculating the inverse of the matrix.
  data <- x$get()
  m <- solve(data,...)
  x$setMatrix(m)
  m
}                                           #Calculates the inverse of the matrix 
