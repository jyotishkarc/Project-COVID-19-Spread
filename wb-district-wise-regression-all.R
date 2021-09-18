library(dplyr)

districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")

districts.df <- as.data.frame(districts.df)
districts.conf <- districts.df %>% filter(State == "West Bengal")

uni.dist <- unique(districts.conf$District)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other State" , ]
uni <- uni[,c(1,3,4)]

uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))

for (i in 1:length(uni.dist)) {
   X[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

M <- matrix(0, nrow = 503, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, nrow(M)-length(X[[k]])), X[[k]])
}

reg.list <- list()

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

districts.cleaned <- as.data.frame(districts.cleaned)

districts.cleaned <- districts.cleaned[151:180,]

# districts.cleaned.centred <- districts.cleaned - 
#    matrix(rep(colMeans(districts.cleaned), nrow(M)), nrow(M), ncol(M), byrow = TRUE)

for (k in 1 : ncol(districts.cleaned)) {
   temp.M <- districts.cleaned
   # temp.M <- districts.cleaned.centred
   
   names(temp.M)[k] <- "current.dep.var"
   reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
   
   rm(temp.M)
}

W <- matrix(0, nrow = ncol(M), ncol = ncol(M))

# temp <- as.numeric(reg.list[[1]]$coefficients)[-1]
# W[1,] <- c(0,temp)

Q <- reg.list[[1]] %>% summary()
H <- Q$coefficients[,4][-1]
G <- rep(0, 23)
G[which(H < 0.05)] <- H[which(H < 0.05)]
temp <- as.numeric(reg.list[[1]]$coefficients)[-1]
W[1,] <- c(0,temp)
W[1,which(G==0)] <- 0

for (k in 2 : (ncol(M)-1)) {
   Q <- reg.list[[k]] %>% summary()
   H <- Q$coefficients[,4][-1]
   G <- rep(0, 23)
   G[which(H < 0.05)] <- H[which(H < 0.05)]
   temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
   W[k,] <- c(temp[1:(k-1)],0,temp[k:(ncol(M)-1)])
   W[k,which(G==0)] <- 0
}

Q <- reg.list[[ncol(M)]] %>% summary()
H <- Q$coefficients[,4][-1]
G <- rep(0, 23)
G[which(H < 0.05)] <- H[which(H < 0.05)]
temp <- as.numeric(reg.list[[ncol(M)]]$coefficients)[-1]
W[ncol(M),] <- c(temp,0)
W[ncol(M),which(G==0)] <- 0

# temp <- as.numeric(reg.list[[ncol(M)]]$coefficients)[-1]
# W[ncol(M),] <- c(temp,0)

W <- as.data.frame(W)
colnames(W) <- uni.dist -> rownames(W)







