w <- ls()
rm(w, list = w)
setwd("D:\\HBMO")
inp <-scan(file="hbmo1.txt")
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

init_pop <- vector("list", 0)
init_pop_temp<-vector("list",0)

init_pop1 <- vector("list", 0)
init_pop2 <- vector("list", 0)
init_pop3 <- vector("list", 0)
init_pop4 <- vector("list", 0)
init_pop5 <- vector("list", 0)
init_pop6 <- vector("list", 0)
init_pop7 <- vector("list", 0)
init_pop8 <- vector("list", 0)
init_pop9 <- vector("list", 0)
init_pop10 <- vector("list", 0)

fit <- vector("numeric", 0)
init_pop_size <- 10
p_pairs <- 0
t_pairs <- 0
speed <- 1
genome_size<-50

for (i in 1:genome_size) {
        v <- sapply(x, function(y) sample(y, 1))
        ##cache <- update_fitness(cache, v)
        ##t_pairs <- sum(sapply(cache, sum))
        ##fit <- c(fit, t_pairs - p_pairs)
        ##p_pairs <- t_pairs
        init_pop1 <- append(init_pop1, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop2 <- append(init_pop2, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop3 <- append(init_pop3, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop4 <- append(init_pop4, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop5 <- append(init_pop5, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop6 <- append(init_pop6, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop7 <- append(init_pop7, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop8 <- append(init_pop8, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop9 <- append(init_pop9, list(c(v)))
}

for (i in 1:genome_size) {
    v <- sapply(x, function(y) sample(y, 1))
    ##cache <- update_fitness(cache, v)
    ##t_pairs <- sum(sapply(cache, sum))
    ##fit <- c(fit, t_pairs - p_pairs)
    ##p_pairs <- t_pairs
    init_pop10 <- append(init_pop10, list(c(v)))
}




queen_bee <- init_pop[[1]]

# mqss_size <- 25
# mqss <- vector("list",0)
# drone_criteria <- vector("numeric",0)
# drone_id <- vector("numeric",0)
# for(i in 1:init_pop_size){
#     drone_criteria <- c(drone_criteria,exp((fit[1]-fit[i])/speed))
#     drone_id <- c(drone_id,i)
#     }
# drone_cri <- data.frame(drone_criteria,drone_id)
# drone_cri <- drone_cri[order(-drone_criteria),]
# drone_id <- drone_cri[,2]
# 
# for(i in 1:mqss_size){
#     mqss<- append(mqss,init_pop[drone_cri[i,2]])
# }
# 
# remove_ids <- drone_id[1:mqss_size]
# init_pop <- init_pop[-remove_ids]
# 
# brood_size <-10
# brood_pop<-sample(mqss,10)
# temp_pop<-vector("list",0)
# ##for(i in 1:brood_size){
#     crossover_ids <- sample(1:20,sample(1:20))
#     temp<-brood_pop[[1]]
#     for(j in 1:length(crossover_ids)){
#             temp[j]<-queen_bee[j]
#     }
#     ##temp_pop<-append(temp_pop,temp)
# ##}