## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The "makeCacheVector" function will create a special "vector", which is really a list containing function to
## 1. set the matrix (minver)
## 2. get the matrix (minver)
## 3. set the value of the inverse matrix (setiver)
## 4. get the value of the inverse matrix (getinver)
makeCacheMatrix <- function(x = matrix()) {
minver<-NULL
## the set () function will take an argument named y, which is assumed to be a numeric vector
## set() and get() are functions to set and access the data values within an object, respectively    
    set<-function(y) {
## the operator "<<-" was introduced to assign a value to an object in an environment that is different from
## the current environment.
## First, set() assigns the input to the "x" function in a different environment
             x<<-y
## then, it will set the NULL value to "minver", and clear cache values of
## "minver" that were previously calculated in other runs by the cacheSolve function,
## That is to ensure that whenever the value of "x" is changed, the existing "minver" is not accessed from a previous calculation, but cleared out
## and recalculated
             minver<<-NULL
    }
    get<-function() x
## Return a matrix that is the inverse (minver) of 'x'
    setinver<-function(inverse) minver<<-inverse
    getinver<-function() minver
## List will create a new object in the element list()
    list(set=set, get=get,
         setinver=setinver,
         getinver=getinver)
}


## Write a short comment describing this function
## the above lis will be the input to the "cacheSolve" function

cacheSolve <- function(x, ...) {
        
## Return a matrix that is the inverse (minver) of 'x'
        
minver<-x$getinver()
    
##Checking if the inverse matrix has been obtained
    if(!is.null(minver)) {
      
##If true, already calculated, skip the calculation
##and get the value from the cache
        message('you will save some time getting the value from cache')
        return(minver)
    }
## If the calculation has not been done yet
## then the calculation of the inverse matrix is done by using the "solve" function
    data<-x$get()
    minver<-solve(data, ...)
## the setting (x$setinver) function will set the value of the inverse matrix in the cache
    x$setinver(minver)
    minver
}
