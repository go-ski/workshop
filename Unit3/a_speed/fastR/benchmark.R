x <- matrix( rnorm( 10000*500 ), nrow = 10000, ncol = 500 )

f <- function( x ) t( x ) %*% x
g <- function( x ) crossprod( x )

library( rbenchmark )
benchmark( f( x ), g( x ) )
