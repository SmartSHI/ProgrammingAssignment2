#The two functions achieved a goal that 
#they can cache the inverse of a matrix.
############################################
#The 1st function , makeCacheMatrix creates
#a special list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(x)
    {
        xrow <- nrow(x)
        temp <- matrix(rep(0,xrow*xrow),xrow,xrow)
        for(i in 1:xrow)
        {
            for(j in 1:xrow)
            {
                if(i == j){temp[i,j] <- 1}
            }
        }
        m <<- solve(x,temp)
    }
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##########################################
##The next function calculates the inverse of 
##the object created with the above function
## it first checks to see if the inverse has 
##already been calculated. If so, it gets the 
##inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data 
##and sets the value of the inverse in the cache 
##via the setinverse function.
cacheSolve <- function(x, ...) 
{
    m <- x$getinverse()
    if(!is.null(m)) 
    {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- x$setinverse(data)
    m
}