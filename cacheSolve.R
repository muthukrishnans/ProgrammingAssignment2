cacheSolve <- function (y, ...)
{
  zInv <- y$GetInv()
  if(!is.null(zInv))
  {
    print("Got cached inverse data")
    print(zInv)
    return(zInv)
  }
  
  z <- matrix(y$Get(), nrow(y$Get()), ncol(y$Get()))
  zInv <- solve(z)
  y$SetInv(zInv)
  print(zInv)
}