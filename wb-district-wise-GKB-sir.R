library(dplyr)

districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")

districts.df <- as.data.frame(districts.df)
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

# reg.list <- list()
# 
# districts.cleaned <- apply(M, 2, function(vec){
#    temp <- c(0, vec[-length(vec)])
#    return(vec - temp)
# })
# 
# X <- districts.cleaned <- as.data.frame(districts.cleaned)[201:260,]

M.v1 <- M[-c(1:13), ]

districts.cleaned.v1 <- apply(M.v1, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

X <- as.data.frame(districts.cleaned.v1)[201:260,]
V <- cov(X)

#G <- matrix(0, nrow = 70, ncol = 23)

W <- matrix(0, nrow = 23, ncol = 23)

for (i in 1:ncol(X)) {
   
   for (j in 1:ncol(X)) {
      
      if (j == i) {next}
      
      V.11 <- V[c(i,j),c(i,j)]
      V.12 <- V[c(i,j),-c(i,j)]
      #V.21 <- t(V12)
      V.22 <- V[-c(i,j),-c(i,j)]
      
      V.11.2 <- V.11 - V.12 %*% solve(V.22) %*% t(V.12)
      
      if(abs(V.11.2[1,2] / V.11.2[1,1]) > abs(V.11.2[1,2] / V.11.2[2,2])){
         W[j,i] <- V.11.2[1,2] / V.11.2[1,1]
      }
      else W[i,j] <- V.11.2[1,2] / V.11.2[2,2]
   }
}

W <- as.data.frame(W)
colnames(W) <- rownames(W) <- uni.dist




















