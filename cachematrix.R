## I followed the logic of the cachemean example:
## initialization of x and inv objects
## assigns the value of y to object x in the parent environment
## assigns the value of NULL to inv in the parent environment, and clears 
## any value of inv that has been cached by a previous execution.

makeCacheMatrix <- function(x = matrix()) {      
        inv <- NULL
        set <- function (y){
        x <<- y
        inv <<- NULL
       }
        get <- function() x                   #returns the value of the array
        setinverse <- function (inverse) inv <<- inverse #set the value of the inverse matrix
        getinverse <- function() inv          #returns the value of the inverse matrix
        list (set = set, get = get,           #make a list of the functions
        setinverse = setinverse,              #set = set, gives the name 'set' 
        getinverse = getinverse)              #to the previously defined set() function 
                                              #and so on
  }

## Return a matrix that is the inverse of 'x'
## extract the getinverse element. Access the function by name instead of using the formula
## check if the result is NULL. 
## every time a new matrix is set on the object, if the value here is not equal to NULL, 
## we have a valid inverse cached and can return it to the main environment
## If the result is FALSE, cachesolve () gets the matrix of the input object, 
## and calculates the inverse matrix, returns the value to the parent environment 
## and prints the inv object


cacheSolve <- function(x, ...) {
          inv <- x$getinverse()        
          if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
          }
          matriz <- x$get()
          inv <- solve (matriz, ...)
          x$setinverse(inv)
          return(inv)
}
