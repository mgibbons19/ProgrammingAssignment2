## "makeCacheMatrix" creates the function and sets the matrix as "x", 
## creates a placeholder for "m", 
## defines a function for "set" 
## defines "x" and "m" in the larger environemnt
## defines "get", "setmatrix", and "getmatrix"

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## "cacheSolve" returns "m" as the inverted matrix of matrix"x"
## "m<-x$getmatrix..." returns the cache, if it's lready solved
## "m<-solve..." solves the invreesion, if there is no cache.
cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
