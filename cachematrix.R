# R Programming Course: 
# assignment 2
# cachematrix.R
# Harald Niederstätter, AUT


rm(list = ls()) ## reset R

## here two functions get defined in the workspace: makeCacheMatrix() and cacheSolve()
## 
## makeCacheMatrix() is a constructor function that takes a (square) matrix as input
## and returns a list harbouring the functions defined within makeCacheMatrix()
## each of these functions will also 'carry along' a link to the environment they were 
## created in 

makeCacheMatrix <- function(x = matrix()) {
      mi <- NULL # the container where the inverted matrix will be stored (cached)
      get <- function() x # retrieve the matrix that makeCacheMatrix was called with 
      set <- function(y) { 
            x <<- y     # set new matrix as input to get it 'up and running' when calling get()
            mi <<- NULL # reset mi as new input matrix is used now
      }
      setinverse <- function(inverse) mi <<- inverse # NOTE: mi lives in setinverse's environ
      getinverse <- function() mi # retrieve inverse matrix from cache
      assign("L4FUN",list(get = get, set = set, setinverse = setinverse, 
                          getinverse = getinverse), pos = 1) # if inexistent L4FUN gets created cacheMatrixes parent.dir       
}

## cacheSolve is a function that calls the functions stored in the list returned by makeCacheMatrix

cacheSolve <- function(x = L4FUN, new = FALSE, y = matrix()) { 
      if(new == TRUE) x$set(y) # if another matrix should be inverted update the cached input matrix
      mi <- x$getinverse() # 'import' the inverse matrix stored in the cache
      if(!is.null(mi)){ # test if the inverse matrix exists and if so 
            message("getting cached data") # drop a note
            return(mi) # output it to cacheSolve's parent directory and quit execution of cacheSolve
      }
      data <- x$get() # in case that the function gets still executed 'import' a copy of the cached input matrix
      mi <- try(solve(data), silent = TRUE) # invert the matrix
      if(is(mi,"try-error")){ #  if inversion is not possible:
            mi <<- NULL # set mi (the one in cacheSolve) to NULL
            return(message("cannot calculate the inverse of matrix")) # drop a note and quit cacheSolve()
      }
      x$setinverse(mi) # if still running write the inverse matrix to the cache
      mi ## and return the inverted matrix to cacheSolve's parent directory
}

# sink("cachematrixOUTPUT.txt") # divert output to file in workingdir
## TEST THE STUFF FROM ABOVE: ###############################################################################
## all that cat() stuff is just to produce a readable output when sourcing the entire script ################ 

cat("CACHING in R: check things out with functions makeCachMatrix() and cacheSolve();\n")
cat("\n") # this prints an empty line
## plan A: we call the creator fun (makeCacheMatrix()) from within cacheSolve() 
cat("first we need to create the list with the four functions by calling makeCacheMatrix():\n")
cat("\n")

cat("we call makeCacheMatrix() directly from the prompt\n")
cat("this creates L4FUN in the workspace (i.e. the parent dir of makeCacheMatrix())\n")
cat("in case we assign the output to a variable (e.g. List) we'll get both L4FUN and List\n")
cat("\n")

cat("List <- makeCacheMatrix()\n")
List <- makeCacheMatrix() # create L4FUN and List
cat("\n")

cat("lets check what now populates the workspace:\n")
print(ls())
cat("\n")

cat("the function call created L4FUN (by default) and File (by demand) in the workspace\n")
cat("L4FUN (List) is a list containing the 4 functions created in the environment of makeCacheMatrix()\n")
cat("these four functions still remember where the environment is where they were born\n")
cat("and this environment still is in the search pathes of these four functions:\n")
cat("\n")

cat("e.g. environment of L4FUN$get():\n")
cat("str(environment(L4FUN$get))\n")
str(environment(L4FUN$get))
cat("\n")

cat("the environment of List$get() matches that of L4FUN$get() etc.:\n")
cat("str(environment(List$get))\n")
str(environment(List$get))
cat("\n")

cat("re-running makeCacheMatrix() will change L4FUN but not List:\n") 
cat("\nmakeCacheMatrix()\n")
makeCacheMatrix()
cat("\n")

cat("now the environment of L4FUN$get() reads:\n")
cat("str(environment(L4FUN$get))\n")
str(environment(L4FUN$get))
cat("\n")

cat("and here again that of List$get() which now mis-matches that of L4FUN$get() etc.:\n")
cat("str(environment(List$get))\n")
str(environment(List$get))
cat("\n")


cat("the objects e.g. in L4FUN$get()'s environment are:\n")
cat("print(ls(environment(L4FUN$get)))\n")
print(ls(environment(L4FUN$get)))
cat("\n")

cat("what is the content of the cached input (the default: matrix())?\n")
cat("print(L4FUN$get())\n")
print(L4FUN$get())
cat("\n")

# check the content of mi in the environment of these four functions
cat("check the content of mi in the environment of these four functions\n")
cat("call L4FUN$getinverse and print mi:\n")
cat("print(L4FUN$getinverse())\n")
print(L4FUN$getinverse())
cat("\n")


# give matrix(NA) - the default for x in makeCacheMatrix - a try
cat("now invert the default value for x (i.e. matrix()):\n")
cat("invM.default <- cacheSolve()\n")
cat("cacheSolve() uses x = L4FUN, new = F, and y = matrix() by default\n")
cat("as new = FALSE, y will be ignored\n")
invM.default <- cacheSolve()
cat(paste0("inverted default matrix(): ")); 
print(invM.default)
rm(invM.default)

# or matrix(NaN)
cat("\n change input to matrix(NaN) and invert it:\n")
cat("invM.NaN <- cacheSolve(L4FUN, new = TRUE, y = matrix(NaN))\n")
invM.NaN <- cacheSolve(new = TRUE, y = matrix(NaN)) 
cat(paste0("inverted matrix(NaN): ")); print(invM.NaN)
cat("\n")
rm(invM.NaN)

cat("did x change? lets retrieve it from cache\n")
cat("x now contains:\n")
print(L4FUN$get())
cat("the content of x changed from NA to NaN due to the call of L4FUN$set(y) in cacheSolve()\n")
cat("\n")

cat("did the content of mi (our cache for the inverted matrix) change?\n")
cat("now the cached value reads:\n")
print(L4FUN$getinverse())
cat("Yep, it got updated as well, here by L4FUN$setinverse()\n")
cat("\n")

# how about matrix(Inf)?:
cat("\ngive matrix(Inf) a try:\n")
# L4FUN <- makeCacheMatrix(matrix(Inf))
invM.Inf <- cacheSolve(new = TRUE, y = matrix(Inf)) # works as L4FUN is the default for x in cacheSolve
cat(paste0("inverted matrix(Inf): ")); print(invM.Inf)
rm(invM.Inf)

# and now a scalar
cat("\nor a scalar:\n")
L4FUN <- makeCacheMatrix(1)
invM.scalar <- cacheSolve(L4FUN, new = TRUE, y = 1) 
cat(paste0("inverted matrix(1): ")); print(invM.scalar)
rm(invM.scalar)

# a character
cat("\na character:\n")
invM.character <- cacheSolve(L4FUN, TRUE, matrix(letters[1])) 
cat(paste0("inverted matrix(letters[1]): ")); print(invM.character)
rm(invM.character)

# a single FALSE
cat("\na single FALSE:\n")
invMat.F <- cacheSolve(L4FUN, TRUE, matrix(FALSE))
cat(paste0("inverted matrix(FALSE): ")); print(invMat.F)
rm(invMat.F)


# or a 3 x 3 logical matrix
cat("\nor 3 x 3 logical array:\n")
invMat.3x3.logical <- cacheSolve(L4FUN, TRUE, matrix(TRUE,3,3))
cat(paste0("inverted matrix(T,3,3): ")); print(invMat.3x3.logical)
rm(invMat.3x3.logical)


# now let's try a non-square matrix
cat("\nnow let's try a non-square matrix: 1st round\n")
set.seed(42)
nsq <- matrix(rnorm(1000000), 500000, 2) # won't work as matrix needs to be square for inversion
cat("invM.nsq.first <- cacheSolve(L4FUN, TRUE, nsq)\n")
invM.nsq.first <- cacheSolve(x = L4FUN, new = TRUE, y = nsq) # should return message that matrix inversion didn't work
cat(paste0("inverted matrix (", nrow(nsq), " x ", ncol(nsq), "): ")); print(invM.nsq.first)


# re-run cacheSolve() and re-feed it with the non-square trouble-maker
cat("\nlet's try the non-square matrix again: 2nd round on the same beast as above\n")
cat("invM.nsq.second <- cacheSolve()\n")
invM.nsq.second <- cacheSolve() # still should return message that matrix inversion didn't work - and nothing else
cat(paste0("inverted matrix (", nrow(nsq), " x ", ncol(nsq), "): ")); print(invM.nsq.second)

rm(nsq) # tidy up a bit


# let's try a matrix that'll work:
cat("\nfeed in a square matrix that will work:")
set.seed(42)
sq1 <- matrix(rnorm(1000000), 1000, 1000) # this one should work

# run cacheSolve() on new matrix sq1 and determine how long the matrix inversion takes #######################################################
cat("\nFirst round on square matrix 1:\n") # some wise words on what's going on
cat("invM.sq1.first <- cacheSolve(new = TRUE, y = sq1)\n")
elapsed1stRun <- system.time(invM.sq1.first <- cacheSolve(new = TRUE, y = sq1)) # do the math and determine how long it takes
cat("str(invM.sq1.first):\n")
cat(str(invM.sq1.first))
cat(paste0("the first round of matrix sq1 inversion (",nrow(sq1), " x ", ncol(sq1), ") took: ", round(elapsed1stRun["user.self"], digits = 3), "s\n")) # print CPU time

rm(sq1, invM.sq1.first)

## re-call cacheSolve() on sq1:
cat("\nSecond round on square matrix 1:\n") # more wise words
cat("re-run cacheSolve() for the same matrix using just default values:\n")
cat("invM.sq1.second <- cacheSolve()\n")
elapsed2ndRun <- system.time(invM.sq1.second <- cacheSolve()) # do it again Sam...
cat("str(invM.sq1.second):\n")
cat(str(invM.sq1.second))
cat(paste0("second round of matrix sq1 inversion - read out cached inverted matrix - took: ", round(elapsed2ndRun["user.self"], digits = 3), "s\n")) # print elapsed

rm(invM.sq1.second)

## and and finally invert a new matrix
cat("\nand now invert a differnt matrix:\n")
set.seed(247)
sq2 <- matrix(rnorm(1000000), 1000, 1000)
cat("invM.sq2 <- cacheSolve(new = TRUE, y = sq2)\n")
elapsedNewMat <- system.time(invM.sq2 <- cacheSolve(new = TRUE, y = sq2)) # keep fingers crossed
cat("str(invM.sq2):\n")
cat(str(invM.sq2))
cat(paste0("input matrix has changed to sq2 - inversion took: ", round(elapsedNewMat["user.self"], digits = 3), "s\n"))

rm(sq2, invM.sq2)

cat("\nCheers,\nHarry\n")
cat("\n")
print(Sys.time())
# sink() # end sink-ing