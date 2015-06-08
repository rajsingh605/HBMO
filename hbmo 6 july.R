##READING OF FILE AND GAINING INFORMATION
w <- ls()
rm(w, list = w)
setwd("C:\\Users\\Home1\\OneDrive\\HBMO")
source("function_hbmo.R")  ##ALL THE FUNCTIONS CALLED ARE IN THIS R SCRIPT
inp <-scan(file="hbmo.txt")

##GENERATING PARAMETERS AND VALUES AS PER THE FILE READ
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

##INITIALIZING ALL VARIABLES
cache <- matrix(rep(FALSE, n * n), ncol = n)
drone_pop_size <- 30
individual_size<- 5
spermatheca_size<-15
fit_drone_pop_genome<-numeric(0)
##fit_drone_pop <- vector("numeric", 0)
fit_drone_pop<-numeric(0)
drone_pop <- matrix(list(),nrow=drone_pop_size,ncol=1)                                                                         
spermatheca_sizepermatheca <- matrix(list(),nrow=spermatheca_size,ncol=1)
p_pairs <- 0
t_pairs <- 0

##GENERATING INITIAL POPULATION
for (i in 1:drone_pop_size){                                                     
    cache <- matrix(rep(FALSE, n * n), ncol = n)
    indi<-numeric(0)                                                            
    for(j in 1:individual_size){                                                
        temp<-sapply(x,function(y) sample(y,1))
        indi<-c(indi,temp)
    }
    cache <- update_fitness(cache, indi)
    t_pairs <- sum(sapply(cache, sum))
      fit_drone_pop<-c(fit_drone_pop,t_pairs)
    drone_pop[[i,1]]<-(indi)
}

cache <- matrix(rep(FALSE, n * n), ncol = n)


queen_index<-which.max(fit_drone_pop)

##relating to indexes of drone population
indexes_dp<-1:30
queen_bee=drone_pop[[queen_index,1]]
indexes_dp<-indexes_dp[-queen_index]
drone_pop<-drone_pop[-queen_index,1]
View(drone_pop)

m<-20000
i_qs<-1
energy<-runif(1,0.5,1)
genome_masked<-numeric(0)

# for(i in 1:10){    
#     t<-0
#     alpha<-0.9
#     for(j in 1:spermatheca_size){
#         ind<-sample(indexes_dp,1)
#         indexes<-indexes_dp[-ind]
#         drone_pop<-drone_pop[-ind]
#         delta_fit_drone_pop<-abs(fit_drone_pop[ind]-fit_drone_pop[queen_index])
#         r<-runif(1,0,1)
#         if((exp((-delta_fit_drone_pop)/energy)<r) && i_qs<16){
#             temp<-drone_pop[ind]
#             spermatheca_sizepermatheca[[i_qs,1]]<-temp
#             i_qs=i_qs+1
#         }
#         t=t+1
#         energy=alpha * energy
#     }
# #     i_qs<-i_qs-1
# #     indexes_qs<-1:i_qs
# #     
#     
# #     for(i in 1:i_qs){
# #         ind_qs<-sample(indexes_qs,1)
# #         indexes_qs<-indexes_qs[-ind_qs]
# #         brood_cand<-spermatheca_sizepermatheca[[ind_qs,1]]
# #         genome_masked<-crossover(queen_bee,brood_cand,cache)
# #     }
# }