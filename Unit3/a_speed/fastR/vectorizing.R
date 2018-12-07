n <- 1e5
x <- seq( 0, 1, length.out = n)
f <- function( x ) exp( x^3 + 2.5*x^2 + 12*x + 0.12 )
y1 <- numeric( n )

set.seed( 12345 )
system.time(
  for( i in 1:n )
    y1[ i ] <- f( x[ i ] ) + rnorm( 1 )
)

set.seed( 12345 )
system.time(
  y2 <- f( x ) + rnorm( n )
)

all.equal( y1, y2 )
