##Below function inverses the passed function and stores in the memory
##.i.e.basically it creates the matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    #sets the value of the m to NULL. Provides the default value 
    m<-NULL
    
    #sets the value of matrix
    setvalueofmatrix<-function(y){
        #cach the value of the matrix
        #use <<- to assign a value to an object in an environment different from the current environment
        x<<-y
        #sets the value of the m to NULL. Provides the default value
        m<<-NULL
    }
    #get the matrics
    get<-function() x
    #set the inverse of the matrix
    setmatrix<-function(solve) m<<- solve
    #get the inverse 
    getmatrix<-function() m
    list(set=setvalueofmatrix, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## Compute the inverse of the matrix returned  by above function
## if the inverse already calculated and not changed then below function retrives the 
## inverse of the matrix directly from the cache 
## otherwise this function creates the inverse of the matrics and stores in the cache
## x is output of makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #compare the previous value to check what was there earlier
    m<-x$getmatrix()
    #check if the inverse has been already calculated
    #if true,rturn the inverse matyrix
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    #if not inverse is not already calculated then calculate it
    matrix<-x$get()
    m<-solve(matrix, ...)
    #store the computed inverse in cache 
    x$setmatrix(m)
    #return the inverse of the matrix
    m
}
