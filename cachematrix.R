## This function takes matrix type argument
## and return special type of list with
## that provides following functions
##1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of matrix
## 4.get the value of the inverse of matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  if(det(x)==0)
  {
    return(NULL)
    
  }
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  } 
  setinverse <- function(inverse){
    
    inv <<- inverse
  } 
  getinverse <- function(){
    inv
  } 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
  
  
}
