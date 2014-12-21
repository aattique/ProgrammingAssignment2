## These functions are used to stores a matrix and 
## cache it's inverse

## The first function, makeCacheMatrix, creates a matrix and
## defines functions to get and set the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
	
	## Set value of the matrix 
        
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 			## get value of matrix
        setinv <- function(inv) m <<- inv	## set value of matrix inverse
        getinv <- function() m			## get value of matrix inverse
	## Make list of the fuctions in the makeCacheMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Following function calculates the inverse of matrix. 
## If inverse has been calculated it is returned from 
## cache, else it is calculated and value set in the cache

cacheSolve <- function(x, ...) {

        m <- x$getinv()					## get matrix inverse
        
	  ## return inverse if in cache

	  if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)				## calculate inverse
        x$setinv(m)					## set inverse in cahce
        ## Return a matrix that is the inverse of 'x'
        m
}

