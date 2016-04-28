## 
## PROGRAMMING ASIGMENT 2: Lexical Scoping
## 

# Creates a list with references to functions for managing a matrix x

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# Uses the previously defined list to cache the computation of the matrix
# inverse

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

# A little test to checkout it is running

testit <- function() {
    M <- matrix(c(1,1,1,2), 2, 2)
    m <- makeCacheMatrix(M)
    stopifnot(M == m$get())
    i <- solve(M)
    i2 <- cacheSolve(m)
    stopifnot(i == i2)
    i3 <- cacheSolve(m)
    stopifnot(i == i3)
}
