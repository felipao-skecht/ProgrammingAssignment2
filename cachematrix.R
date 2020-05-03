## This project contains two functions 1) makeCacheMatrix (creates a special "matrix" object that can cache its inverse) 
## and 2) cacheSolve (computes the inverse of the special "matrix" returned by makeCacheMatrix).    


## The makeCacheMatrix function creates a special "matrix" which is really a list containing 
        #other functions to perform the following tasks:
        # 1) Set the value of the matrix
        # 2) Get the value if the matrix
        # 3) Set the inverse of the matrix
        # 4) Get the inverse of the matrix

makeCacheMatrix<- function(x=matrix()) {
     
  
        inv<-NULL
        # 1) Set the value of the matrix
        set <- function(y) {
                x<<-y
                inv<<-NULL
        }
           # 2) Get the value if the matrix
        get<- function() x
         # 3) Set the inverse of the matrix
        setinverse<-function(inverse) inv <<- inverse
             # 4) Get the inverse of the matrix
        getinverse <- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
       

##cacheSolve function calculates the inverse of the special “matrix” created with the makeCacheMatrix function. 
        #It first checks to see if the inverse of the matrix has already been calculated. 
        
cacheSolve <- function(x, ...) {
        #Return a matrix that is the inverse of x
        
        inv <- x$getinverse()
        
        #In this case, it gets the inverse from the cache and skips the computation. If not, it calculates the inverse of 
        #the data and sets the inverse matrix in the cache via the setinverse function.

        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
