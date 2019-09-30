## Matrix inversion is a costly compution and there could be benefit to caching the inverse of a matrix vs. computing repeatedly
## Assignment#2: pair of functions that will cache inverse of a matrix

## creates a special matrix object that can cache its matrix

makeCacheMatrix <- function(x = matrix()) {
    
  #initialize inverse object
      inverse<-NULL
      
  #setting the matrix
      set<-function(matrix){
        x<<-matrix
        inverse<<-NULL
      }
  #getting the matrix from cache
      get<-function() x
      
  #setting the inverse matrix
      set_inv<-function(Inv_solve) inverse<<-Inv_solve
      
  #getting the inverse matrix 
      get_inv<-function() inverse
      
  #creating the special "matrix" object
      list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
  
  
}


## Function computes the inverse of the special matrix object returned by  makeCacheMatrix function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        
    #getting inverse from cache
        inverse<-x$get_inv()
   
     #check if null, and return inverse if already set
      if(!is.null(inverse)){
          message("getting cache data")
          return(inverse)
      }
    
    #getting matrix
      matrix_temp<-x$get()
    
    #solving the matrix
       inverse<-solve(matrix_temp)
    
    #setting the solved matrix
       x$set_inv(inverse)
    
    #returns solved matrix
       inverse
  
}
