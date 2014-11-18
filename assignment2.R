makeCacheMatrix <- function(x = matrix()){ #function input will be in the form of a matrix
	m <- NULL #local variable 'm' is declared and assigned a value of NULL
	set <- function(y){
		x <<- (y) #variable x in the containing environment is updated to be y
		m <<- NULL #variable m in the containing environment is updated to be NULL
	}
	get <- function() x #stores the matrix in a command so that the cacheSolve can access this input if the inverse has not been calculated
	setinverse <- function(solve) m <<- solve #the function will use the solve command which finds the inverse of the matrix
	getinverse <- function() m 
	list(set = set, get = get, #a list to store the variables generated
		setinverse = setinverse,
		getinverse = getinverse)
}


cacheSolve <- function(x, ...){ #function input will be in the form of a matrix
	m <- x$getinverse() #obtains value for the makeCacheMatrix functions
	if(!is.null(m)){ #if statement that will be executed if the inverse of the given data matrix has already been calculated
		message("getting cached inverse") #this message is printed to allow the user to know that the inverse has already been calculated and the cacheSolve function is merely displaying that result
		return(m) #the inverse matrix will be displayed
	}
	data <- x$get() #if the inverse matrix has not already been calculated then this command will store the matrix in a variable called
	m <- solve(data, ...) #the inverse matrix will be calculated and stored in variable m
	x$setinverse(m) #reaffirms that m will be the inverse
	m #prints the inverse matrix
}
