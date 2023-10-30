get_sum <- function(a, b) {
  return( a + b )
}
get_squared <- function(a) { 
  return( a^2 )
}

var_one <- get_sum(2, 4)
var_one <- get_squared(var_one)

var_one <- 
  get_sum(2, 4) %>%
  get_squared()
