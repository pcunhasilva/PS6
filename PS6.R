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

sg.int<-function(g,...,lower, upper, inParallel = FALSE, cores = 4){
   
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
fn1 <- function(x){
   x[1]^4 + 2*x[2]
}

fn2 <- function(x){
   x[1]^4 + 2*x[2] + x[3]^2
}

fn3 <- function(x){
   x[1]^4 + 2*x[2] + x[3]^2 + x[2]^5
}

######################################
############ Unit tests ### ##########
######################################

# Evaluate that the area of the curve has to be always positive
test_that("Area is always positive",
          expect_gte(sg.int(fn1, lower = c(0, 0), upper = c(2, 2)), 0))
# Evaluate that sg.int.hi.dim should produce a similar result to adaptIntegrate
test_that("Produce a similar approximation", 
          expect_equal(as.vector(sg.int(fn2, lower = c(0, 0, 0), upper = c(5, 5, 5))),
          adaptIntegrate(fn2, lowerLimit = c(0, 0, 0), upperLimit = c(5, 5, 5))$integral))
# Evaluate that sg.int.hi.dim produces a vector (the test has to fail)
test_that("Is a vector",
          expect_is(sg.int(fn1, lower = c(2, 3), upper = c(3, 5)), "numeric"))

#####################################
############ Measure Speed ##########
#####################################

# Compare the sg.int.hi.dim with the adaptIntegrate (Precision test)
#sg.int.hi.dim(fn2, lower = c(0, 0, 0), upper = c(5, 5, 5)) - 
#   adaptIntegrate(fn2, lowerLimit = c(0, 0, 0), upperLimit = c(5, 5, 5))$integral