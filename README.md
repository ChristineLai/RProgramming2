# RProgramming2
#R Programming 2 - To create cache for the inverse of a square matrix
# This is my repository that I set up on 1/2212015
##Assignment 2: Part 1-create a special "matrix" object that can cache its inverse
##by Christine Lai
makeCacheMatrix <- function(x=marix()){
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list (set = set, get=get, 
              setsolve = setsolve,
              getsolve = getsolve)
        
}

cacheSolve <- function (x, ...) {
        m <- x$getsolve()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
}


#testing
#need to have set up the function makeVector and cachemean
#a <- makeCacheMatrix()  #initialize
#a                       #shows that a is a list of functions
#class(a)                #a is a list
#class(a$set)            #set(within a) is a function
#a$set(matrix(c(4,4,-2,2,6,2,2,8,4), 3,3))          #set the function, specify the matrix
#a$set(matrix(c(2,3,-2,1,2,2,4,2,3),3,3))          #set the function, specify the matrix
#a$get()                 #set the function
#cacheSolve(a)            #returns the matrix that was previously "set"
#cacheSolve(a)            #to get the result

#end of the codes
