## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Below function inverses the passed function and stores in the memory

makeCacheMatrix <- function(x = matrix()) {
    #sets the value of the m to NULL. Provides the default value 
    m<-NULL
    
    #sets the value of matrix
    setvalueofmatrix<-function(y){
        #cach the value of the matrix
        x<<-y
        #sets the value of the m to NULL. Provides the default value
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=setvalueofmatrix, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## Write a short comment describing this function
##below function first checks the inverse of the matrics in the memory ,if not availalbe the creates the 
## inverse of the passed matrics 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #compare the previous value to check what was there earlier
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    #compute the inverse of the matrix 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    #return the inverse of the matrix
    m
}
