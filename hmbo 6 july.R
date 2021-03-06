w <- ls()
rm(w, list = w)
setwd("C:\\Users\\HOME1\\OneDrive\\HBMO")
inp <-scan(file="hbmo.txt")
strength <- inp[1]
parameters <- inp[2]
p_list <- inp[3:length(inp)]
x <- vector("list",0)
q <- 0;
for(i in 1:length(p_list)){
  x <- append (x, list((length(unlist(x)) + 1):(q+p_list[i])))
  q <- q + p_list[i]
}
n <- length(unlist(x))
cache <- matrix(rep(FALSE, n * n), ncol = n)

update_fitness <- function(cache, v) {
  for ( i in 1:length(v)) {
    for (j in 1:length(v)) {
      cache[v[i], v[j]] <- TRUE
      cache[v[j], v[i]] <- TRUE
    }
  }
  return(cache)
}

fit <- vector("numeric", 0)
init_pop_size <- 30 ## population size=30
p_pairs <- 0
t_pairs <- 0
speed <- 1
