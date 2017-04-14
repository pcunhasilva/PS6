###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 6
## Author: Patrick Cunha Silva
## Tasks: 1 - Comment the original function.
## Tasks: 2 - Increase dimensionality to 3.

rm(list = ls())

# We have the original function. We will add comments to it.
sg.int<-function(g,...,lower,upper){
  require("SparseGrid") # Load the Sparse Grid Library
  lower<-floor(lower) # Round the number to the downwards (i.e 9.8 is rounded to 9)
  upper<-ceiling(upper) # Round the number to the upwards (i.e 9.8 is rounded to 9)
  if (any(lower>upper)) stop("lower must be smaller than upper") # Stop the function and return a
  # error messeger if the value in the vector lower is greater than the one in the vector upper.
  gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),seq(lower[2],upper[2]-1,by=1)))
  # Generate a object gridss of class matrix that contains all possible combinations between
  # the values of the sequences created using the vectors lower and upper with increments of 1.
  sp.grid <- createIntegrationGrid('KPU', dimension=2, k=5 ) # Create an object named sp.grid
  # that contains two items. The first one is nodes, that is a matrix with a node in each row
  # and the second is weights, that is a vector with the corresponding weights.
  nodes<-gridss[1,]+sp.grid$nodes # Sum the row 1 of the gridss matrix and the nodes
  # matrix of the object sp.grid.
  weights<-sp.grid$weights # Copy weights vector (component of the object sp.grid)
  # to a new vector named weights
  for (i in 2:nrow(gridss)){ # start a loop over the rows of the matrix gridss
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes) # Bind by rows the matrix nodes with
    # a matrix formed by the sum of row i of the matrix gridss to the matrix nodes of
    # the object sp.grind.
    weights<-c(weights,sp.grid$weights) # concatenate the vector weights to the 
    # vector weights in the object sp.grid. It will result in a vector of lenght 
    # 196 (number of items in weight (49) multiplied by the number of loops (4)) 
  }
  gx.sp <- apply(nodes, 1, g,...) # Run the function g for each colunm in the object nodes.
  val.sp <- gx.sp %*% weights # Multiply the matrix gx.sp by the vector weights
  val.sp # Display the 1 by 1 matrix val.sp that contains the area of the curve.
}

