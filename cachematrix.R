makeCacheMatrix <- function(x = matrix()) {	## Creates a special matrix object that can cache its inverse
        # makeCacheMatrix is creates a list containing functions to 
			# 1. set the matrix
        		# 2. get the matrix
        		# 3. set the inverse
        		# 4. get the inverse
        i = NULL	## Initializing the inverse property
        set = function(y) {
        # '<<-' used to assign a value to an object in an environment that is different from the current environment. 
        x <<- y
        i <<- NULL
        }
        get = function() x	## To get the matrix
        setinv = function(inverse) i <<- inverse 	## To set the inverse of the matrix
        getinv = function() i		## To get the inverse of the matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
	}

cacheSolve <- function(x, ...) {	## Calculates the inverse of the original matrix and sets the inverse via setinverse function
        i = x$getinv()
        # if the inverse has already been calculated
        if (!is.null(i)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(i)
        }
        # otherwise, calculates the inverse 
        mat.data = x$get()
        i = solve(mat.data, ...)
        x$setinv(i)		## Set the  inverse in the cache
        return(i)
	}
