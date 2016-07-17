## Put comments here that give an overall description of what your
## functions do

##This function produces a list of four functions which can be called on a matrix.
#The functions make a cache matrix (a matrix stored in the cache of the system), return the matrix, cache the inverse of the matrix and print the inverse matrix 

        makeCacheMatrix <- function(x = matrix()) {
                
                #creation of a variable to store the cache value of the matrix. This value is initially defined a NULL.
                cacheValue <- NULL
                
                        #Option 1 in the list to be defined: this function makes a matrix 
                        #this function stores the value of a matrix as input into the makeCashMatrix function.
                        setMatrixValue <- function(matrixValue){
                                
                                #assigns the value of matrixValue to x. This value is useable in the parent environment of setMatrixValue.
                                x <<- matrixValue
                                
                                #the matrix value, as stored in the cache, is set back to null. This is assigned in the parent environment.
                                cacheValue <<- NULL
                        }
                        
        #Option 2 in the list to be defined: this function retrieves the matrix defined
        getMatrix <- function(){
                
                #prints the matrix stored in makeCashMatrix input (x)
                print(x)
        }
        
        #Option 3 in the list to be defined: stores the inverse of a matrix in the cache
        cacheInverseMatrix <- function(y){
                
                #stores the inverse of a matrix into cache which is assigned outside of the function
                cacheValue <<- y 
                
        }
        
        #Option 4 in the list to be defined: returns the cache inverse of the defined matrix
        getInverseMatrix <- function(){
                
                #returns the value of the cache matrix
                return(cacheValue)
        }
        
        # returns a list of functions as defined in the above options
        list(makeCacheMatrix = makeCacheMatrix, getMatrix = getMatrix, cacheInverseMatrix = cacheInverseMatrix, getInverseMatrix = getInverseMatrix) 
        
        #end of function makeCacheMatrix
}


##This function returns the inverse of a special square matrix as defined by the makeCacheMatrix function

cacheSolve <- function(z, ...) {
        
        #gets the inverse of a the matrix
        inverse <- z$getInverseMatrix()
        
                #assesses whether a cache value exists for this matrix and returns it if it does (if invsere is not empty, then the inverse is returned)
                if(!is.null(inverse)) {
                        
                        #returns a message stating the cache value is being used
                        message("Returning cache data")
                        
                        #returns the cache stored inverse matrix
                        return(inverse)
                
                }
        
                #if there is no value in the inverse, the function calculates the inverse of the matrix
                else{
                        #gets the value of the matrix and stores it in a variable matrix
                        matrix <- z$getMatrix()
                        
                        #stores the inverse of the matrix into the inverse variable
                        inverse <- solve(matrix)
                        
                        #stores the value into cache matrix
                        z$cacheInverseMatrix(inverse)
                        
                }
        
        #prints the output of the inverse matrix
        print(inverse)

        #end of cacheSolve function
}
