## makeCacheMatrix creates  a  "matrix" object m that can set,get and calcualate the inverse
## the matrix mInv. if the inverse is in the cache , it will return it or 
## if not found, it will calucate and return the same.  Matrix is assumed to be square and invertible .
makeCacheMatrix <- function(m = matrix()) {
        
        mInv<-NULL
        set <- function(y) {
                m <<- y
                mInv <<- NULL
        }
        get <- function() m
        setMatInverse<- function() mInv <<-solve(m)
        getMatInverse <- function() mInv
        list(set = set, get = get,
             setMatInverse = setMatInverse,
             getMatInverse = getMatInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve  
## retrieves the inverse from the cache.
cacheSolve <- function(m, ...) {

        mInv <- m$getMatInverse() ## Return a matrix that is the inverse of 'x'
        if (!is.null(mInv)) {
                message("getting cached inverse matrix")
                return(mInv)
        } else {
                mInv <- solve(m$get())
                m$setMatInverse(mInv)
                return(mInv)
        }
}
