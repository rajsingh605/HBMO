update_fitness <- function(cache, v) {
    for ( i in 1:length(v)) {
        for (j in 2:length(v)) {
            if(v[i]!=v[j]){
                cache[v[i], v[j]] <- TRUE
                cache[v[j], v[i]] <- TRUE
            }
        }
    }
    return(cache)
}



crossover <- function(x,y){    
    for(i in 1:1){
        cache <- matrix(rep(FALSE, n * n), ncol = n)
        cache1<-update_fitness_genome(cache,y[i:i+2])
        print(cache1)
        t_pairs <- sum(sapply(cache1, sum))
        fit <- c(fit, t_pairs - p_pairs)
        p_pairs <- t_pairs
        fit_genome<-c(fit_genome,t_pairs)
    }
    genome_to_be_masked<- which(fit_genome == min(fit_genome))
    return(genome_to_be_masked)
}



# update_fitness <- function(cache, v) {
#     for ( i in 1:length(v)) {
#         for (j in 2:length(v)) {
#             if(v[i]!=v[j]){
#                 cache[v[i], v[j]] <- TRUE
#                 cache[v[j], v[i]] <- TRUE
#             }
#         }
#     }
#     return(cache)
# }
# 
# update_fitness_genome <- function(cache,v) {
#     for ( i in 1:length(v)) {
#         for (j in 2:length(v)) {
#                 cache[v[i], v[j]] <- TRUE
#                 cache[v[j], v[i]] <- TRUE
#             }
#         }
#     return(cache)
# }
# 
# crossover <- function(x,y,cache){    
#     for(i in 1:1){
#         temp<-y[i:i+2]
#         cache<-update_fitness_genome(cache,temp)
#         t_pairs <- sum(sapply(cache, sum))
#         fit <- c(fit, t_pairs - p_pairs)
#         p_pairs <- t_pairs
#         fit_genome<-c(fit_genome,t_pairs)
#     }
#     genome_to_be_masked<- which(fit_genome == min(fit_genome))
#     return(genome_to_be_masked)
# }
