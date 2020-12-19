# This file implements the core matrix operations for the least squares computation.

# Not used in the applet - however, this combines the individual operations to solve the problem in its entirety.
find_ls_sol <- function(predictor, y) {
  X <- augment_X(predictor)
  return(beta(X, y))
}

# Augments the matrix of regressors by binding a column of 1s to the left.
augment_X <- function(x) {
  return(cbind(1, x))
}

# Multiplies X transpose by X. (t(X) transposes the matrix and %*% does matrix multiplication.)
XtX <- function(X) {
  return(t(X) %*% X)
}

# Finds the inverse of XtX. (solve() finds the inverse of a matrix.)
XtXinv <- function(X) {
  return(solve(XtX(X)))
}

# Multiplies X transpose by Y.
XtY <- function(X, Y) {
  return(t(X) %*% Y)
}

# Computes the least squares coefficients (betas) using the previous functions.
beta <- function(X, Y) {
  return(XtXinv(X) %*% XtY(X, Y))
}

# Computes the fitted Y values by matrix multiplication.
fitted_Y <- function(b, X) {
  return(X %*% b)
}

# Computes the residuals (difference between fitted Y and real Y). 
residuals <- function(fitted_Y, Y) {
  return(Y - fitted_Y)
}