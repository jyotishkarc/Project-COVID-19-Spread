library(dplyr)



districts.df <- as.data.frame(districts.df)
districts.conf <- districts.df %>% filter(State == "West Bengal")


uni.dist <- unique(districts.conf$District)

uni <- districts.conf[districts.conf[,3] != "Unknown" || districts.conf[,3] != "Other State" , ]
uni <- uni[,c(1,3,4)]

uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))

for (i in 1:length(uni.dist)) {
   X[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

M <- matrix(0, nrow = 503, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0,503-length(X[[k]])), X[[k]])
}

M.part <- M[1:30,]
reg.list <- list()

# for(i in 1:23){
#    reg.list[[i]] <- lm(M.part[,i]~ . , data = M.part)
# }


districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})


####################

# result <- lm(M[,6] ~ . , data = as.data.frame(M))
result <- lm(M[,4] ~ M[,4] + M[,6] + M[,9] + M[,23])

















