##Cache the inverse of a Matrix

## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # prepared to store the value of inverse
  set <- function(y){ ##set the value of the matrix
    x <<- y #把y赋值给matrix x
    m <<- NULL
  }
  get <-function() x #get返回x的值，即get等于x
  setinverse <- function(inverse) m <<- inverse #让m等于inverse
  getinverse <- function() m #让getinverse等于m，即缓存inverse
  list(set = set,get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated(and the matrix has not changed), then the 
##cachesolve should retrieve the inverse of a matrix.
cacheSolve <- function(x, ...) {# x is the special "matrix" returnd by makeCacheMatrix above
  m <- x$getinverse #把cachematrix里的inverse值赋给m
  if (!is.null(m)){ #如果m不是空，表明inverse已缓存，函数返回缓存的inverse值
    message("getting cache data")
    return(m)
  }
  data <- x$get() #如果m为空，表明没有缓存的inverse，让data等于需要计算inverse的matrix
  m <- solve(data) #计算该matrix的inverse,把值赋给m
  x$setinverse(m) 
  m
}
