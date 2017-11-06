makeCacheMatrix<- function(x=matrix()) {
        inv<- NULL
        get<- function() x
        setinv<- function(inv_m) inv<<-inv_m
        getinv<- function() inv
        list(get=get,setinv=setinv,getinv=getinv)
}

cacheSolve<- function(x,...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return (inv)
        }
        data <- x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}

tm<- matrix(c(1,1,1,2,3,4,0,1,5),3,3)
solve(tm)

mcm<-makeCacheMatrix(tm)
mcm$get()
mcm$getinv()

imcm<-cacheSolve(mcm)
imcm