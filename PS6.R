###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 6
## Author: Patrick Cunha Silva
## Tasks: 1 - Comment the original function.
## Tasks: 2 - Increase dimensionality and add parallel option
## Tasks: 3 - Generate tests.

rm(list = ls())
library(testthat)
library(cubature)
library(microbenchmark)

############################################################################
############ Do parallel and increase Dimensionality Function ##############
############################################################################

sg.int <- function(g,...,lower, upper, inParallel = FALSE, cores = 4){
   
   require("SparseGrid") # Load the Sparse Grid Library
   require("plyr") # call the package plyr to use parallel aaply
   require("doMC") # Load doMc library to define the number of cores.
   if(inParallel){
      registerDoMC(cores = cores) # Define number of cores.
   }
   
   if(length(lower)!=length(upper)) stop("lower and upper must have the same length") # test if
   # the vector lower and the vector upper have the same length. 
   dimensions <- length(upper) # store the number of dimensions.
   
   lower<-floor(lower) # Round the number to the downwards (i.e 9.8 is rounded to 9)
   upper<-ceiling(upper) # Round the number to the upwards (i.e 9.8 is rounded to 9)
   if (any(lower>upper)) stop("lower must be smaller than upper") # Stop the function and return a
   # error messeger if the value in the vector lower is greater than the one in the vector upper.
   seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to")) # Vectorize the function seq.
   gridss<-as.matrix(unique(expand.grid(lapply(lower, function(x)seq2(x, upper-1, by=1)))))
   # Generate a object gridss of class matrix that contains all possible combinations between
   # the values of the sequences created using the vectors lower and upper with increments of 1.
   sp.grid <- createIntegrationGrid('KPU', dimension = dimensions, k=5 ) # Create an object named sp.grid
   # that contains two items. The first one is nodes, that is a matrix with a node in each row
   # and the second is weights, that is a vector with the corresponding weights. 
   nodes<-gridss[1,]+sp.grid$nodes # Sum the row 1 of the gridss matrix and the nodes
   # matrix of the object sp.grid.
   weights<-sp.grid$weights # Copy weights vector (component of the object sp.grid) to a new vector named weights
   for (i in 2:nrow(gridss)){ # start a loop over the rows of the matrix gridss
      nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes) # Bind by rows the matrix nodes with
      # a matrix formed by the sum of row i of the matrix gridss to the matrix nodes of
      # the object sp.grind.
      weights<-c(weights,sp.grid$weights) # concatenate the vector weights to the 
      # vector weights in the object sp.grid.
      }
   gx.sp <- aaply(.data = nodes, .margins = 1, .fun = g, .parallel = inParallel, ...) 
   # Run the function g for each colunm  in the object nodes. Use parallel if inParallel is equal TRUE.
   val.sp <- gx.sp %*% weights # Multiply the matrix gx.sp by the vector weights
   val.sp # Display the 1 by 1 matrix val.sp that has the value of the area of the curve.

}
######################################
############ Test functions ##########
######################################

# Generate functions to use in the tests
# Two Dimensions
testfn1 <- function(x){
   x[1]^4 + 2*x[2]
}
# Three Dimensions
testfn2 <- function(x){
   x[1]^4 + 2*x[2] + x[3]^2
}
# Four Dimensions
testfn3 <- function(x){
   x[1]^4 + 2*x[2] + x[3]^2 + x[2]^5
}
# Function to test accuracy (One Dimension)
testfn4 <- function(x){
   (.7*dchisq(x, df = 4)+.3*dchisq(x, df = 6))
}
# Answer to testfn4 (Integration from 4 to 10)
ans_testfn4 <- .7*(pchisq(10, df = 4)-pchisq(4, df = 4))+
   .3*(pchisq(10, df = 6)-pchisq(4, df = 6))

######################################
############ Unit tests ### ##########
######################################

# Evaluate that the area of the curve has to be always positive
test_that("Area is always positive",
          expect_gte(sg.int(testfn1, lower = c(0, 0), upper = c(2, 2)), 0))
# Evaluate that sg.int.hi.dim should produce a similar result to adaptIntegrate
test_that("Produce a similar approximation", 
          expect_equal(as.vector(sg.int(testfn2, lower = c(0, 0, 0), upper = c(5, 5, 5))),
          adaptIntegrate(testfn2, lowerLimit = c(0, 0, 0), upperLimit = c(5, 5, 5))$integral))
# Evaluate that sg.int.hi.dim produces a vector (the test has to fail)
test_that("Is a vector",
          expect_is(sg.int(testfn1, lower = c(0, 0), upper = c(5, 5)), "numeric"))


#################################
######### Compare Speed #########
#################################

# Compare speed of integration of a function with two dimensions from 0 to 5
microbenchmark(
   "sg.int: No parallel Two Dim" = sg.int(testfn1,
                                          lower = c(0, 0), 
                                          upper = c(5, 5)),
   "sg.int: Parallel Two Dim." = sg.int(testfn1, 
                                        lower = c(0, 0), 
                                        upper = c(5, 5), 
                                        inParallel = TRUE),
   "adaptIntegrate Two Dim." = adaptIntegrate(testfn1, 
                                              lowerLimit = c(0, 0), 
                                              upperLimit = c(5, 5)),
   times = 100L
)

# Compare speed of integration of a function with Three dimensions from 0 to 5
microbenchmark(
   "sg.int: No parallel Three Dim" = sg.int(testfn2, 
                                            lower = c(0, 0, 0), 
                                            upper = c(5, 5, 5)),
   "sg.int: Parallel Three Dim." = sg.int(testfn2, 
                                          lower = c(0, 0, 0), 
                                          upper = c(5, 5, 5), 
                                          inParallel = TRUE),
   "adaptIntegrate Three Dim." = adaptIntegrate(testfn2, 
                                                lowerLimit = c(0, 0, 0), 
                                                upperLimit = c(5, 5, 5)),
   times = 100L
)

# Compare speed of integration of a function with Four dimensions from 0 to 3
microbenchmark(
   "sg.int: No parallel Four Dim" = sg.int(testfn3, 
                                           lower = c(0, 0, 0, 0), 
                                           upper = c(8, 8, 8, 8)),
   "sg.int: Parallel Four Dim." = sg.int(testfn3, 
                                         lower = c(0, 0, 0, 0), 
                                         upper = c(8, 8, 8, 8), 
                                          inParallel = TRUE),
   "adaptIntegrate Four Dim." = adaptIntegrate(testfn3, 
                                               lowerLimit = c(0, 0, 0, 0), 
                                                upperLimit = c(8, 8, 8, 8)),
   times = 100L
)

#####################################
######### Compare Precision #########
#####################################

# Compare accuracy of sg.int with the adaptIntegrate
sg.int(testfn4, lower = c(4), upper = c(10)) - ans_testfn4
adaptIntegrate(testfn4, lowerLimit = c(4), upperLimit = 
                  c(10))$integral - ans_testfn4 # More accurate.

################################
######### Optmization  #########
################################

# Generate the function
f_op <- function(x) {
   sin((x[1]^2)/2 - (x[1]^2)/4) * cos((2*x[1])-exp(x[1]))
}

# The Defaulf of optim is to minimize the function. 
# to maximize, control$fnscale must be negative.
maxfunc <- optim(par = c(2, 1), fn = f_op, lower = c(-1, 1), upper = c(3, 3), 
      method = "L-BFGS-B", control = list(fnscale = -1))

# Show the pair (x, y) that maximize the function:
maxfunc$par

# Show the value of the function at x and y values that maximize the function:
maxfunc$value

# Generate a function that may take two arguments to use in optimize,
f_op2 <- function(x, y) {
   sin((x^2)/2 - (y^2)/4) * cos((2*x)-exp(y))
}

# Calculate f(x, y)
objective <- unlist(lapply(seq(-1, 3, by = 0.01), function(j) optimize(f = f_op2, y = j, 
                                                             lower = -1, upper = 3, 
                                                             maximum = TRUE)$objective))

# Find the value of f(x, y) at the maximum of the function in the interval
objective[which.max(objective)]

# Calculate the maximum for the function
maximum <- unlist(lapply(seq(-1, 3, by = 0.01), function(j) optimize(f = f_op2, y = j, 
                                                                     lower = -1, upper = 3, 
                                                                     maximum = TRUE)$maximum))
# Find the maximum of the function
maximum[which.max(maximum)]

# Show the values of x and y for max of f(x, y)
paste("y =", seq(-1, 3, by = 0.01)[which.max(maximum)], "x =", round(maximum[which.max(maximum)], 2))
