library(dplyr)

n=5
districts.conf <- districts.df %>% filter(State == "West Bengal")

uni.dist <- unique(districts.conf$District)
uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other State" , ]
uni <- uni[,c(1,3,4)]
uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))

H <- list()

for (i in 1:length(uni.dist)) {
   H[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

M <- matrix(0, nrow = 503, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, nrow(M)-length(H[[k]])), H[[k]])
}

M.v1 <- M[-c(1:13), ]

districts.cleaned.v1 <- apply(M.v1, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

G <- matrix(0, nrow = floor(490/n), ncol = 23)

for(i in 1:23){
   for(k in 1:floor(490/n)){
      G[k,i] <- sum(districts.cleaned.v1[(n*k-n+1):(n*k),i])/n
   }
}

G <- as.data.frame(G)
colnames(G) <- uni.dist
rownames(G) <- sapply(1:floor(490/n), function(val){paste("w",val, sep = " ")})