##the makeCacheMatrix function creates a matrix list that holds both the matrix and the inverse
makeCacheMatrix <- function(incoming) {
     actualmatrix<-incoming
     inversematrix<-solve(incoming)
     list(actualmatrix=actualmatrix,inversematrix=inversematrix)
}
     
## Return a matrix that is the inverse of an incoming matrix or CacheMatrix.
cacheSolve <- function(x, ...) {
     ##I could not get a mu
     if ((class(x)!="matrix" && class(x)!="list") |(class(x)=="list" && !all(names(x)==c("actualmatrix","inversematrix")))) {
          message("invalid object class passed to cacheSolve. Please pass CacheMatrix or matrix.")
        return("")
     }
     if(class(x)=="matrix") {
          message("matrix passed. Getting inverse. [Inverse will not be cached.]")
          inv<-solve(x)
     } 
     if(class(x)=="list" &&  is.null(x$inverse)){
          if(is.null(x$inversematrix)){
               message("null CacheMatrix passed. Please pass valid CacheMatrix")
               return()
          }
          message("CacheMatrix passed with null Inverse. Getting inverse. [Inverse will not be cached.]")
          inv<-solve(x)  
     }
     if(class(x)=="list" && !is.null(x$inverse)){
          message("Getting CacheMatrix inverse")
          inv<-x$inverse
     }
     inv  
}
