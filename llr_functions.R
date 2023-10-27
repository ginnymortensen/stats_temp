llr <- function(x, y, z, omega) {
  fits <- sapply(z, compute_f_hat, x=x, y=y, omega=omega)
  return(fits)
}

compute_f_hat <- function(z, x, y, omega) {
  Wz = make_weight_vector(z, x, omega)
  
  Wz_times_X = t(apply(X, 1, function(row) row * Wz))
  Wz_times_y = Wz * y
  
  f_hat = c(1, z) %*% solve(t(X) %*% Wz_times_X) %*% t(X) %*% Wz_times_y
  
  return(f_hat)
}


make_weight_vector <- function(z, x, omega) {
  # Compute weights for each x relative to z
  weights = sapply(x, function(xi) {
    r = abs(xi - z) / omega
    if(r < 1) return((1 - r^3)^3)
    else return(0)
  })
  return(weights)
}


make_predictor_matrix <- function(x) {
  return(cbind(1, x))
}

# # Uncomment the following if the 'french_fries' dataset is from a specific library
# # library(your_library_name_here)
# library(reshape2)
# data(french_fries)
# french_fries = french_fries[complete.cases(french_fries),]
# z = seq(0, 15, length.out = 100)
# fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 2)
# plot(z, fits)
# # Assuming the french_fries dataset is loaded in your environment:
# french_fries <- french_fries[complete.cases(french_fries), ]
# z <- seq(0, 15, length.out = 100)
# fits <- llr(x = french_fries$potato, y = french_fries$buttery, z = z, omega = 2)
# plot(z, fits, type="l", main="Locally Weighted Regression Fit", xlab="Potato", ylab="Buttery")

