## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix" # creates a special "matrix" object that can 
#   cache its inverse.  This is achieved via creating a list containing 
#  1.  a function to set the value of the matrix
#  2.  get the value of the matrix
#  3.  set the value of the inverse
#  4.  get the value of the inverse
#   


makeCacheMatrix <- function(x = matrix()) {
  
  if (length(grep("MASS", (.packages())))==0)  {
    library(MASS)     # for computing generalized inverse, their `ginv` is 
    #  a  convenient     interface to `svd`
  }
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvrs <- function(inv) m <<- inv
  #     if(dim)
  #     m <<- expression(obj)
  #   }
  getinvrs <- function() m
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## "cacheSolve" calculates the inverse of a matrix
#  it first checks to see if the inverse has already been calculated. 
#  If so, it `get`'s the inverse  from the  cache and skips the computation. 
#  Otherwise, it attempts to calculate  the inverse of
#  the data and sets the value of the inverse in the cache via the "setinvrs"
#  function

cacheSolve <- function(x, ...) {
  
  m <- x$getinvrs()
  if(!is.null(m)) {
    message("getting cached inverse of a matrix")
    return(m)
  }
  data <- x$get()
  
  cmpInv <- function(dt) {
    out <- tryCatch(  { if (dim(dt)[1]==dim(dt)[2])  solve(data, ...)  
                        else
                          ginv(dt,...) 
    },
    error= function(cond) {
      #  message(paste("Your matrix:", as.character(head(data))))
      message("Failed to invert supplied matrix; original error message:")
      message(cond)                    
      return(NA)  # Choose a return value in case of error
    } 
    )
    return(out)
  }
  
  m <- cmpInv(data) 
  if (is.numeric(m))  {
    x$setinvrs(m)   # cache the obtained inverse
    ## Return a matrix that is the inverse of 'x'
    return(m)   
  }
  else    return(out)   # NA, the message has been generated
  #   and sent to stderr()  connection
  
}

  ################# Usage:   ##################
 #  Regular pos. def. matrix:
 # hilbert <- function(n) { i <- 3:n; 1 / outer(i - 1, i, "+") }
 # h4 <- hilbert(6) 
 # specM <- makeCacheMatrix(h4)
 #  t1 <- cacheSolve(specM2)

  # Rectangular matrix: 

     #  m2 <- matrix( abs(rnorm(12, 250,38)),3,4) 
     # specM2 <- makeCacheMatrix(m2)
   #  chk <- cacheSolve(specM2)
