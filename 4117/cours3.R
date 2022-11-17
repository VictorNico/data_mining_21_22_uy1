Mode <- function(v){
  set <- unique(v)
  nbr_apparution <- rep(0, length(set))
  for(element in set){
    nbr_apparution[set==element] <- length(v[v==element])
  }
  return (set[nbr_apparution==max(nbr_apparution)])
}
rv <- c(11, 18, 19, 21, 29, 46, 21, rep(1,5), rep(2,5) )
m <- Mode(rv)
m