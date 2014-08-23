#Summary

# The functions in this assignment match up very closely with the example provided by the instructor with regards to
# code setup and structure.   The primary difference being that the below functions calculate the inverse of an
# (invertible) matrix (instead of the mean of vector components), and cache the value of the inverse matrix 
# for future recall (instead of caching the 'mean' as provided in the instructor's vector example)

## NOTE:  As per Assignment Instructions, all matrices provided are assumed to be invertible, so no extra validation
##        steps were taken to test this.



#  makeCacheMatrix() - INFO

## The makeCacheMatrix() function takes a given, invertible matrix and creates a 4 element List object
## This List object contains the objects: "set", "get", "setsolve", "getsolve".  
## When the function is called for the first time it stores the values of the matrix provided to it, 'x', as 
## well as initializing the value of 'm' to NULL.   

## THe critical thing, however, is the function's use of the '<<-' operator.
## This allows the variable values to 'persist' in a given environment, which allows for re-assignment in environments
## other than the global environment.   
## When calling this function  an 'environment' value is automatically added to the 4 list objects 
## (e.g. <environment: 0x00000000092cb350> - for example )
## When the cacheSolve() function is then called, it will search the environment in this memory space and 
## then either request a cached value, or update a value of one of the variables in this environment (for future recall).


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



#  cacheSolve() - INFO

## This function checks the values in the List object created by the makeCacheMatrix().
## It first calls the List's getsolve() function (which returns the value for 'm').   

## If the value for 'm' is NULL (it would be on the first call, as makeCacheMatrix() initializes 'm' to NULL),
## it will then create a variable 'data' and assign it the value of the matrix returned by the List's get() function.
## R's solve() function is then applied in order to generate the inverse matrix.   
## This "inverse matrix" value is then stored locally in the 'm' variable (of this function), and then passed back  
## via the List Object's setsolve() function, which then "caches" the 'm' value stored in the List Object.
## At this point the local 'm' variable is passed as output (i.e. the inverse matrix requested)

## When cacheSolve() is subsequently called for the same List Object, it will check the value for 'm' via the
## List object's getsolve() function, which should now, no longer be NULL.   In this situation, a message noting
## retrieval of "cached data" is provided, and the 'm' value (the inverse matrix) is returned.



cacheSolve <- function(x, ...) {

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


# Notes:

## These functions were tested on several invertible matrices.

## The first was a 2x2 matrix with row1 [4,3] and row2 [3,2].
## A list object was then created using the makeCacheMatrix() which was then passed to the cacheSolve() function.
## The correct inverse matrix was then returned (row1 [-2,3], row2 [3,-4]).  On subsequent cacheSolve() execution
## the correct cached value was returned (including the message noted in the code).

## The second matrix tested was a 2000x2000 matrix.   
## After creating the list object (via makeCacheMatrix()), the cacheSolve() function was applied and took ~7 seconds
## to return the inverse matrix on the initial call.   All subsequent calls, using the cached value for the inverse,
## took under 0.1 seconds to return the inverse matrix.   The returned inverse matrix was confirmed as correct, after
## multiplying the inverse matrix by the initial matrix (using matrix multiplication - %*%, and rounding to 4 decimals). 
## The identity matrix was returned.


##  Find below the results for the 2x2 matrix:

# > A <- matrix(rbind(c(4,3),c(3,2)),2)
# > A
# [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# > cache_A <- makeCacheMatrix(A)
# > names(cache_A)
# [1] "set"      "get"      "setsolve" "getsolve"
# > Inverse_A <- cacheSolve(cache_A)
# > Inverse_A
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > ## 2nd run through, should now be cached and show the cached message
# > Inverse_A_2nd <- cacheSolve(cache_A)
# getting cached data
# > Inverse_A_2nd
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > ## To confirm Inverse_A is the inverse of A, use matrix multiplication
# > A %*% Inverse_A
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > ## Which is the Identity Matrix
# 

