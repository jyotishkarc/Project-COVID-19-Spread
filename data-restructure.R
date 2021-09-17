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

reg.list <- list()

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

districts.cleaned <- as.data.frame(districts.cleaned)

# districts.cleaned.centred <- districts.cleaned - 
#    matrix(rep(colMeans(districts.cleaned), nrow(M)), nrow(M), ncol(M), byrow = TRUE)

for (k in 1 : ncol(districts.cleaned)) {
   temp.M <- districts.cleaned
   # temp.M <- districts.cleaned.centred
   
   names(temp.M)[k] <- "current.dep.var"
   reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
   
   rm(temp.M)
}

W <- matrix(0, nrow = nrow(districts.cleaned), ncol = nrow(districts.cleaned))

# for (k in 1 : ncol(districts.cleaned)) {
#    W[k,] <- c()
# }









