##This file contains two functions: makeCacheMatrix and cacheSolve.
#They are two functions that are used to create a special object that 
#stores a matrix  and cache's its inverse.

##The main goal of them is to store a matrix variable and calculate
##its inverse only if the matrix we are refering to have not been changed.

##Example:
        
        ##If we have the nxn identity matrix (the one with 1 in the
        ##diagonal and 0 in the other components) stored in the variable "Id" and
        ##we apply the makeCacheMatrix function on it, and store: 
        ##Id_matrix <- makeCacheMatrix(Id), then if we call cacheSolve(Id_matrix),
        ##then the inverse of "Id", that in this case is the same as "Id", is returned.
        ##If now immediately we call again, cacheSolve(Id_matrix), then, because of
        ##the fact that the inverse of "Id" have been calculated before, and stored 
        ##in the caché, the computer does not calculate it again, and the message
        ##"getting cached data" is returned (also the stored inverse calculated before 
        ## is returned).
        
        ##If we now redefine the Id_matrix variable to another matrix "M", i.e. we
        ##do Id_matrix <- makeCacheMatrix(M) (for instance: M <- matrix(c(1,2,3,4), nrow = 2))
        ##then when we call cacheSolve(Id_matrix), the inverse of the matrix M is returned
        #and stored in the caché. If we next call cacheSolve(Id_matrix) again,
        ##then the computer does not calculate the inverse of the vurrent M again, 
        ##because it is stored.. (x_inv is not NULL, see the functions so as to understand).


## makeCacheMatrix() function takes a matrix object as an argument. 
## We must check that class(x) = matrix.

#This function creates a special "matrix", which is really a list containing a function to

       # 1. set the value of the matrix
       # 2. get the value of the matrix
       # 3. set the value of the matrix's inverse
       # 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        #The variable that will contain the inverse of x is initialized to NULL
        x_inv <- NULL
                
        set <- function(y) {
                x <<- y
                #It indicates that x have been updated and we 
                #haven't got its inverse actualized.
                x_inv <<- NULL   
        }
        
        get <- function() x #get function returns de current matrix x.
        
        # set the "inverse", inverse matrix of x, to the x_inv variable.
        setinverse <- function(inverse) x_inv <<- inverse 
        #the variable x_inv is returned by getinverse function.
        getinverse <- function() x_inv
        #A list of the four functions is returned.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)     
                
}


## cacheSolve() function takes a list object as an argument. 
## We can check that class(x) = list.
        #In our case, the argument x will usually be the one returned by the 
        #makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinverse()
        if(!is.null(x_inv)) {#Here it is checked if we have the inverse 
                             #of the current x stored in x_inv or not
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get() #the matrix x is stored in the data variable.
        x_inv <- solve(data, ...) #we use here solve function to find 
                                  #the inverse of x
        x$setinverse(x_inv) #We store the new inverse calculated before and stored in x_inv.
                            #so as not to calculate it again in future calls.
        x_inv #The inverse of x, stored in x_inv is returned.
        
}
