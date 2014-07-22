## This R code is for R programming Assignment 2 
## It will be divided into 2 functions - "makeCacheMatrix" and "cacheSolve"
## To avoid computing the inverse of matrix repeatedly, 
## the functions will cache the inverse and return a message which the cache is retrieved
## Output example from R below (at the end of the code)

## makeCacheMatrix will create a special "matrix" object 
## that can cache the inverse

makeCacheMatrix <- function(x = matrix()) 
{
	inv_mat<-NULL
  	set<-function(y)
	{
  		x<<-y
  		inv_mat<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) inv_mat<<- solve
	getinverse<-function() inv_mat
	list(set=set, get=get,
   	setinverse=setinverse,
   	getinverse=getinverse)
}

## For the first time, cacheSolve will calculate the inverse of the matrix 
## which is given by the function "makeCacheMatrix".
## If the matrix has not changed and we try to calculate the inverse for the second time   
## it will omit the steps for calculation and return the inverse from the cache

cacheSolve <- function(x=matrix(), ...) 
{
	inv_mat<-x$getinverse()
    	if(!is.null(inv_mat))
	{
      	message("getting cached data")
      	return(inv_mat)
    	}
    	matrix<-x$get()
    	inv_mat<-solve(matrix, ...)
    	x$setinverse(inv_mat)
    	## Return a matrix that is the inverse of 'x'
	inv_mat
}

##Output for R:
##> source("cachematrix.R")
##> x <-makeCacheMatrix()
##> x$set(matrix(rnorm(16),4,4))
##> cacheSolve(x)
##           [,1]       [,2]       [,3]       [,4]
##[1,] -0.1792152  0.1689088 -0.1845496 -0.1817100
##[2,] -0.6458917 -0.1311861 -0.4073938  0.9118983
##[3,] -0.2434362  0.5749265  0.2658014 -0.1398690
##[4,] -0.2912141 -0.1848780  0.4017307 -0.2668100
##> x$get()
##             [,1]        [,2]       [,3]       [,4]
##[1,] -1.317058092 -0.61780733 -0.1237325 -1.1496887
##[2,]  0.005614754 -0.07726930  1.3982158 -1.0008958
##[3,] -2.234044370 -0.07549956  0.8945950  0.7944758
##[4,] -1.930123442  0.61417884  0.5131725 -0.6033716
##> cacheSolve(x)
##getting cached data
##           [,1]       [,2]       [,3]       [,4]
##[1,] -0.1792152  0.1689088 -0.1845496 -0.1817100
##[2,] -0.6458917 -0.1311861 -0.4073938  0.9118983
##[3,] -0.2434362  0.5749265  0.2658014 -0.1398690
##[4,] -0.2912141 -0.1848780  0.4017307 -0.2668100
##> x<-(matrix(rnorm(16),4,4))
##> matx <-makeCacheMatrix(x)
##> cacheSolve(matx)
##          [,1]       [,2]      [,3]      [,4]
##[1,] -1.104101 -0.5146123 -1.043732  1.315566
##[2,] -3.371881 -0.9486741 -5.400291  4.561385
##[3,]  2.809968 -0.2832764  4.491074 -3.658079
##[4,]  1.429935  1.1977368  3.385266 -5.059211


