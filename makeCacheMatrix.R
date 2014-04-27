makeCacheMatrix <- function(x = matrix)
{
  c = ncol(x)
  r = nrow(x)
  
  xInv <- NULL
  
  set <- function(y = matrix)
  {
      x <<- y
      xInv <- NULL
  }
  
  get <- function() x = matrix(x, r, c)
  setInv <- function(x)
  {
    xInv <<- x
  }
  getInv <- function() xInv
  
  list(Set = set, Get = get,
       SetInv = setInv,
       GetInv = getInv)
}