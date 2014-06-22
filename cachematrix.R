## This function caches a matrix
## Call the makeCacheMatrix function to cash a matrix
## This is done by using the 4 functions: set, get, setsolve, getsolve
## The function returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {

	# create a matrix to be cashed
        
	m <- NULL

      set <- function(y) {

	# environment is set for the matrix

             x <<- y
             m <<- NULL

        }

	 # set the functions to solve the cached matrix	
	 # environments are set for the inverse matrix

       get <- function() x

       setsolve <- function(solve) m <<- solve

       getsolve <- function() m

	 # list with 4 functions is returned

       list(set = set, get = get,

             setsolve = setsolve,

             getsolve = getsolve)

}

# This functions calculates the inverse (solves) of a cashed matrix
# The function first checks it the matrix has been cashed
# If the matrix is cashed, this cashed matrix is used
# If the matrix is not cashed, the solve function is performed 'as normal'

cacheSolve <- function(x, ...) {

	  #check if matrix is already been solved and cashed or not

        m <- x$getsolve()

	  #if matrix is cashed, this is retrieved

        if(!is.null(m)) {

                message("getting cached data")

                return(m)

        }

	  #matrix is not cashed, calculate inverse with solve
	  #first get the original matrix

        data <- x$get()
	  
	  #solve the matrix

        m <- solve(data, ...)

        x$setsolve(m)

	  #return inverse matrix

        m
}

