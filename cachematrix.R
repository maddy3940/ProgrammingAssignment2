
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                          #creates a special 'matrix' object that can cache its inverse
  m <- NULL               #set an empty inverse matrix
  set <- function(y) {
                          #sets the value of the matrix (x==y) to the parent env (<<-)
    x <<- y
    m <<- NULL            #set an empty inverse matrix in the parent env (<<-)
  }
  get <- function() x     #gets the value of the matrix
  setinv <- function(solve) m <<- solve           #sets the value of the inverse matrix (m) in the parent env (<<-)
  getinv <- function() m                          #gets the value of the inverse matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  #computes the inverse of the special "matrix" returned by makeCacheMatrix above
  #if the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache
  
  m <- x$getinv()      #gets the value of the inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
                       #if m already exists (the inverse is already calculated) then return m value from cache
  }
                       #if m doesn't exist, calculate the inverse of the matrix
  data <- x$get()      #gets the value of the matrix
  m <- solve(data, ...)       #calculates the inverse
  x$setinv(m)
  
  ##now set this newly calculated inverse matrix in the parent env
  ##chache it so the next time you won't have to recalculate
  
  m                   #return the inverse matrix
  
}
