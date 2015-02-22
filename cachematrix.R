## Function is designed to calculate and cache (i.e. store) the inverse matrix of some specified matrix. In case of further 
## calculations the inverse matrix need not to be calculated again as it is retrived from memory.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL ##sets the variable 's' that will hold inverse matrix to NULL
        set <-function(y){ ##function that sets 'x' matrix to the new 'y' matrix and resets 's' (sets to NULL)
                x<<-y
                s<<-NULL
        }
        
        get<-function() x ##function that reutrns initial matrix 'x'
        setinverse<-function(solve) s<<-solve ##function that stores inverse matrix (i.e. attributes inverse matrix to 's')
        getinverse<-function() s ##function that returns inverse matrix that is stored in 's'
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##creates list of functions that will be used 
                                                                                ## in the next function
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinverse() ##attributes inverse matrix to 's'
        if(!is.null(s)) { ##checks if 's' is not NA i.e. if it has a value; if it has, function reurns cached value
                message("getting cached data")
                return(s)
        }    
        data <- x$get() ##attributes initial matrtix 's' to 'data' variable
        s <- solve(data, ...) ##calculates inverse matrix based on 'data' variable
        x$setinverse(s) ##attributes inverse matrix to s
        s ##prints 's' so it returns a matrix that is the inverse of 'x'
}
