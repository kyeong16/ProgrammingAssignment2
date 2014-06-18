## makeCacheMatrix allows you to "store" a matrix so that you can recall it later
## Let x be a matrix. then the code mat<-makeCacheMatrix(x) stores
## the matrix x. 
## Let y be another matrix. 
##  The function mat$set(y) allows you to replace the matrix x with another matrix y. 
##  mat$get() returns the matrix that is "stored"
##  mat$setInverse(inverse) allows you to input the inverse of the stored matrix
##  mat$getInverse() returns the inverse that is stored

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x <<-y
                m<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) m<<-inverse
        getInverse<-function()m
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)       
}


## cacheSolve returns the inverse of the "stored" matrix x in mat using mat$setInverse.
## Otherwise, it computes and returns the inverse of x using the solve function.

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)){
                message("getting Inverse of Matrix")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setInverse(m)
        m
}
