library(dplyr)

# districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")

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

V <- as.data.frame(cov(X))
colnames(V) <- rownames(V) <- uni.dist

#G <- matrix(0, nrow = 70, ncol = 23)

pref <- matrix(0, nrow = 23, ncol = 23)

for (i in 1:ncol(X)) {
   for (j in 1:ncol(X)) {
      
      if (j == i) {next}
      
      V.11 <- V[c(i,j),c(i,j)]
      V.12 <- V[c(i,j),-c(i,j)]
      #V.21 <- t(V12)
      V.22 <- V[-c(i,j),-c(i,j)]
      
      V.11.2 <- V.11 - as.matrix(V.12) %*% solve(V.22) %*% t(V.12)
      
      if(abs(V.11.2[1,2] / V.11.2[1,1]) > abs(V.11.2[1,2] / V.11.2[2,2])){
         pref[j,i] <- 1
      }
      else pref[i,j] <- 1
   }
}

reg.list <- list()

# for (k in 1 : ncol(districts.cleaned)) {
#    temp.M <- districts.cleaned[ ,c(k, which(pref[k,] == 1))] %>% as.data.frame()
#    
#    names(temp.M)[1] <- "current.dep.var"
#    reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
#    
#    rm(temp.M)
# }
# 
# W <- list()
# 
# for (k in 1 : ncol(M)) {
#    Q <- reg.list[[k]] %>% summary()
#    H <- Q$coefficients[,4][-1]
#    G <- rep(0, 23)
#    G[which(H < 0.05)] <- H[which(H < 0.05)]
#    temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
#    
#    if (k == 1) {W[[k]] <- c(0, temp)}
#    if (k == ncol(M)) {W[[k]] <- c(temp, 0)}
#    else W[[k]] <- c(temp[1:(k-1)],0,temp[k:(ncol(M)-1)])
#    
#    W[[k]][which(G==0)] <- 0
# }

W <- data.frame(NA, 23, 23)

for (k in 1 : ncol(districts.cleaned)) {
   print(k)
   
   temp.M <- districts.cleaned[ ,c(k, which(pref[k,] == 1))] %>% as.data.frame()
   # temp.M <- districts.cleaned.centred
   # print(names(temp.M)[k])
   names(temp.M)[k] <- "current.dep.var"
   reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
   Q <- reg.list[[k]] %>% summary()
   p <- Q$coefficients[,4][-1]
   y <- 0
   
   if(max(p) < 0.05){
      temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
      s <- rownames(Q$coefficients)[-1]
      W[k,s] <- Q$coefficients[,1][-1]
      print(reg.list[[k]] %>% summary())
   }
   else{
      while (max(p)>=0.05 & y < 21) {
         y <- y+1
         s <- rownames(Q$coefficients)[1+which.max(p)]
         temp.M <- temp.M[,-which(names(temp.M) %in% s)]
         reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
         Q <- reg.list[[k]] %>% summary()
         p <- Q$coefficients[,4][-1]
         if(max(p) < 0.05){
            temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
            s <- rownames(Q$coefficients)[-1]
            W[k,s] <- Q$coefficients[,1][-1]
            print(reg.list[[k]] %>% summary())
         }
      }
   }
}


# W <- as.data.frame(W)
# colnames(W) <- rownames(W) <- uni.dist




















