w <- ls()
rm(w, list = w)
setwd("C:\\Users\\Rajvardhan\\OneDrive\\HBMO")
source("function_hbmo.R")
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

fit <- vector("numeric", 0)
init_pop_size <- 30                                                            ##initial population size
individual_size<- 5                                                            ##size of one individual(would consist 
##of this many test cases(genomes))

# update_fitness <- function(cache, v) {
#     for ( i in 1:length(v)) {
#         for (j in 2:length(v)) {
#             if(v[i]!=v[j]){
#             cache[v[i], v[j]] <- TRUE
#             cache[v[j], v[i]] <- TRUE
#         }
#     }
#     }
#     return(cache)
# }

cache <- matrix(rep(FALSE, n * n), ncol = n)

fit_genome<-numeric(0)


init_pop <- matrix(list(),nrow=init_pop_size,ncol=1)
p_pairs <- 0
t_pairs <- 0
q_s<-15                                                                         ##queen spermatheca size
q_spermatheca <- matrix(list(),nrow=q_s,ncol=1)


for (i in 1:init_pop_size){                                                     ## this loop
    cache <- matrix(rep(FALSE, n * n), ncol = n)
    indi<-numeric(0)                                                            ## here creates the intial
    for(j in 1:individual_size){                                                ## drone population
        temp<-sapply(x,function(y) sample(y,1))
        indi<-c(indi,temp)
    }
    cache <- update_fitness(cache, indi)
    t_pairs <- sum(sapply(cache, sum))
    #     fit <- c(fit, t_pairs - p_pairs)
    #     p_pairs <- t_pairs
    fit<-c(fit,t_pairs)
    init_pop[[i,1]]<-(indi)
}

queen_index<-which.max(fit)

##relating to indexes of drone population
indexes_dp<-1:30
queen_bee=init_pop[[queen_index,1]]
indexes_dp<-indexes_dp[-queen_index]

m<-20000
i_qs<-1
energy<-runif(1,0.5,1)

for(i in 1:10){    
    t<-0
    alpha<-0.9
    for(j in 1:q_s){
        ind<-sample(indexes_dp,1)
        indexes<-indexes_dp[-ind]
        delta_fit<-abs(fit[ind]-fit[queen_index])
        r<-runif(1,0,1)
        if((exp((-delta_fit)/energy)<r) && i_qs<16){
            temp<-init_pop[[ind,1]]
            q_spermatheca[[i_qs,1]]<-temp
            i_qs=i_qs+1
        }
        t=t+1
        energy=alpha * energy
    }
    i_qs<-i_qs-1
    indexes_qs<-1:i_qs
    
    for(i in 1:i_qs){
        ind_qs<-sample(indexes_qs,1)
        indexes_qs<-indexes_qs[-ind_qs]
        brood_cand<-q_spermatheca[[ind_qs,1]]
        crossover(queen_bee,brood_cand)
    }
}