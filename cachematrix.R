##########################################################################
#----------    Programming Assignment 2: Lexical Scoping      ------------
## The two functions, when combined, allow the program to retrieve a complex
## calculation from cache instead of recalulating and tying up resources.
## Matrices can be quite complex, and inverting them could take too many resources.
## This function establishes a matrix, as well as the conditions to retrieve 
## values in the parent environment for future coding. If the matrix has been
## changed, it will set m to null, so that subsequent formulas do not retrive
## incorrect cached data.
makeCacheMatrix <- function(x = matrix())
 {
   inv <- NULL
   set <- function(y) {
     x <<- y 
     inv <<- NULL
   } 
   
   get <- function() x
   setInv <- function(inverse) inv <<- inverse
   getInv <- function() inv
   list(
     set = set, 
     get = get, 
     setInv = setInv, 
     getInv = getInv
       )
}

#-------------------------------------------------------------------------
## This function first tests whether a cache of data has been established.
## If not, it inverts the matrix and stores the value in m.
## If so, it retrieves the cached data.

cacheSolve <- function(x, ...) 
  {
    inv <- x$getInv()
    if(!is.null(inv)) 
      {
        message("getting cached result")
        return(inv)
      }
    Math <- x$get()
    inv <- solve(Math, ...)
    x$setInv(inv)
    inv
  }		  

## ----------------------Checking the program------------------------------
#Example #1

my_matrix1 <- matrix(rnorm(16),4,4)
ex1_matrix <- makeCacheMatrix(my_matrix1)
cacheSolve(ex1_matrix)
###-- Result 

##          [,1]        [,2]        [,3]       [,4]
##[1,] -0.5184181  0.43352695  0.53868047 -0.2801646
##[2,]  1.0946290 -0.68463157 -0.67986870 -0.6938027
##[3,] -0.3228856  0.02842346 -0.06316416 -0.3718601
##[4,]  0.3162943 -0.55679889  0.24116993  0.5839725

#Example #2

my_matrix2 <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix2$get()

##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

cacheSolve(my_matrix2)

##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
