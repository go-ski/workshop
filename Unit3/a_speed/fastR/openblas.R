x <- matrix( rnorm( 1e6*2e2 ), nrow = 1e6 )

system.time( y <- crossprod( x ) )

library( openblasctl )

openblas_set_num_threads( 1 )
system.time( y <- t( x ) %*% x )
system.time( z <- crossprod( x ) )

openblas_set_num_threads( 2 )
system.time( y <- t( x ) %*% x )
system.time( z <- crossprod( x ) )

openblas_set_num_threads( 4 )
system.time( y <- t( x ) %*% x )
system.time( z <- crossprod( x ) )
