## The function will cache the inverse of a matrix

## the makeCacheMatrix() function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {             ##input x is a matrix
        
        m<-NULL ## m is the inverse matrix that is reset to NULL every time makeCacheMatrix is called
        
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        
        ##the next three functions are defined in the makeCacheMatrix function and are used 
        ##by the cahceSolve function to get the values of x and set the inverse matrix
        
        get<-function () {x}   ##this function returns the value of the original matrix x
        setsolve<-function (solve) {m <<-solve}  ##this is called by cacheSolve during the 
                                                ##first cacheSolve access
        getsolve<-function () {m}      ##this function returns the cached value to 
                                        ##cacheSolve on subsequent accesses 
       
        list(set=set,get=get,        ##internal functions(methods). They are accessed each time makeCacheMatric 
             setsolve=setsolve,      ##is called indicate how the calling function should access these methods
             getsolve=getsolve)     
             

}


## This function computes the inverse of the special matrix returned by makeCacheMean.
##if the inverse has already been calculated then the cacheSolve function should retrieve the inverse
##from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'-the matrix object 
                                  ##created by makeCacheMatrix()
        
        m<-x$getsolve() ##accesses the object matrix x and creates its inverse matrix
        
        if (!is.null(m)) {                          ##if the cache is not empty 
                messages("getting cached data")     ##send message to console
                return(m)                           ##return the cached inverse matrix
        }
        
        data<-x$get()   ##this is reached only if x$getsolve() returned NULL
        m<-solve(data,...) ##if m was NULL calculate inverse matrix
        x$setsolve(m)      ##store calculated inverse matrix in x 
        m                  ##return the inverse matrix of x 
}
