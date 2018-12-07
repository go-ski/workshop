x <- matrix(rnorm(20000*750), nrow=20000, ncol=750)

system.time(t(x) %*% x)

system.time(crossprod(x))

system.time(cov(x))
