## This function perform matrix inversion of the given matrix. This 
# function used concept of caching the inverse of a matrix. 
# first it will checks to see if the matrix inversion has already been calculated. 
# If so, it gets the matrix inverion from the cache and skips the computation. 
# Otherwise, it calculates the matrix invesion of the data and sets the value 
# of the invesersion in the cache via the setmean function.


# Put comments here that give an overall description of what your
## 

## Function Description. This function create the cacheMatrix (Special matrix)
# It uses Closer fuctionaly implement catche operation.
## It create the 4 objects (set, get, getinverse, setinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)

}


##  This function first check the matrix invertable conditions.
## It will check whether it is having any cache if so it use cache 
## objects perform inversion. otherwise it creat cache objects 
## This function retruns matrix invesrion

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c_inv <- x$getinverse()
    if(!is.null(c_inv)) {
        message("getting cached data")
        return(c_inv)
    }
    data <- x$get()
    if ((dim(data)[1] == dim(data)[2]) && !is.null(data)) {
        if (det(data) != 0) {
            c_inv <- solve(data, ...)
            x$setinverse(c_inv)
            c_inv
        } else {
            print(" This is a non invertable matrix")
            
        }
    } else {
        print( " This is a non square matrix")
        
    }
    c_inv

}


## Example varification

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

## set the cache by running makeCacheMatrix()
s_matrix <- makeCacheMatrix(n1)

## call cacheSolve to extract the matrix inversion

cacheSolve(s_matrix)


