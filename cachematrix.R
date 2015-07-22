makeCacheMatrix <- function(x = matrix()) {	## Creates a special “matrix” object that can cache its inverse
        ## makeCacheMatrix is creates a list containing functions to 
			## 1. set the matrix
        		## 2. get the matrix
        		## 3. set the inverse
        		## 4. get the inverse
        inv = NULL
        set = function(y) {
        # '<<-' used to assign a value to an object in an environment that is different from the current environment. 
        x <<- y
        inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
	}

cacheSolve <- function(x, ...) {	## cacheSolve return the inverse of the original matrix input to makeCacheMatrix()
        
	inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(inv)
        }
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        return(inv)
	}
