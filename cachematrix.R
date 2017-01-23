## As per the assignment, the matrix supplied should always a invertible square matrix


## n1 and n2 accept the dimensions from user from console. Dimensions should be equal 
##as above mentioned(say 2x2). The matrix entered at cacheSolve() should also have same 
##dimensions.To test with other dimensions(say 3x3), kindly change the n1,n2 values as
##well and then call cacheSolve


n1<-as.numeric(readline("Enter the nrows of matrix:"))
n2<-as.numeric(readline("Enter the ncols of matrix:"))

mt<-matrix(0,nrow = n1,ncol = n2)  ##create the initial matrix with all zeroes
m<-NULL   ##create the initial variable m which is the inverse matrix

##function to create the list of tasks to be done by the cacheSolve function
makeCacheMatrix<-function(x=matrix()){
  ##set initializes the global variable mt with the new matrix(if the matrices differ) and m to NULL
  set <- function(y) {
    mt <<- y
    m<<-NULL
  }
  
  get <- function(){x}  ##function to get matrix entered at cacheSolve
  setinv <- function(inv){m<<-inv} ##function to cache the inverse matrix
  getinv <- function(){m}  ##function to get the inverse matrix
  
  return(list(set=set,get = get,
              setinv = setinv,
              getinv = getinv))
}



## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ##checks if the matrices are same. If yes, then cache inverse matrix is returned. 
  if(all(x$get()==mt)){
    message("Getting cached Inverse Matrix:")
    m <- x$getinv()
    return(m)
    
  }
  
  ##Else Inverse matrix is calculated.
  matrx<- x$get()
  x$set(matrx)
  m <- solve(matrx)
  x$setinv(m)
  cat("Inverse of matrix:","\n")
  m  ##returns the inverse
}

##Eg:- 
##>cacheSolve(makeCacheMatrix(matrix(2:5,2,2)))
##Inverse of matrix: 
#      [,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1
##>cacheSolve(makeCacheMatrix(matrix(2:5,2,2)))
##Getting cached Inverse Matrix:
#      [,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1
##>cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
##Inverse of matrix: 
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5