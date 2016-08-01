## MakeCachedMatrix returns a list which is similar to a class in other languages. 
## it includes set and get functions for getting the value of the matrix and reseting it respectively
## it includes get.inverse and set.inverse functions for getting the value of an inverse and setting it respectively


makeCacheMatrix <- function(x = matrix())
{
  
  ## Preliminary error handling: Check if the input is a matrix
  ## Check if that matrix is a square matrix
  
  if (is.matrix(x))
  {
    if(dim(x)[1]!=dim(x)[2])
    {
      stop("The matrix is not square")
    }
  }
  else
  { stop("The input must be a matrix")}
  
  x.inverse <- NULL
  
  ## set function to reset the matrix
  set <- function(y) {
    x <<- y
    x.inverse <<- NULL
  }
  
  # get function to get the original matrix
  get <- function() x
  
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) x.inverse <<- solve
  get.inverse <- function() x.inverse 
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}

## cachesolve is a function that takes a chacheable matrix
## checks if the inverse is already cached, returns it if so
## else calculates the inverse and caches it to the originally supplied cacheable matrix

cacheSolve <- function(chached.x, ...) 
  
{
  ## check if an inverse was already calculated
  temp<-chached.x$get.inverse()
  if(!is.null(temp))
  {
    message("Getting Cached inverse matrix")
  ## if it is, retrieve it rather than calculate again
    return(temp)
  }
  ## else, calculate it and save it in cache
  else
  {
    temp<-solve(chached.x$get())
    chached.x$set.inverse(temp)
    return(temp)
  }

}

## sample code, use this to test the matrix
## x<-matrix(c(1,2,3,4),2,2) ## to create a 2 by 2 matrix
## chached.x<-makeCacheMatrix(x)
## cacheSolve(cached.x)
## cached.x$get()%*%cached.x$get.inverse() ## should return a matrix with diagonal of 1 and 0s elsewhere

