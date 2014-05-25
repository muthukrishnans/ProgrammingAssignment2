## Functions to Cache the Inverse of a Matrix

## There are two functions

## 1. makeCacheMatrix <- function(x = matrix) 
## Above function creates a special "matrix" object X that can cache its inverse

## 2. cacheSolve <- function (y, ...)
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Returns a list having function Set, Get, SetInv and GetInv
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

## Checks for Inverse matrix data in env, if not available then try to create it and print
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