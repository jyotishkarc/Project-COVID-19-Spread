library(nleqslv)
library(magrittr)
library(dplyr)

districts.df <- read.csv(file.choose(),header = TRUE)

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

M <- matrix(0, nrow = 554, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, nrow(M)-length(X[[k]])), X[[k]])
}

reg.list <- list()

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

districts.cleaned <- as.data.frame(districts.cleaned)

f <- function(p){
   d <- 23
   V.eps.inv.1= diag(1/p[(d*(d-1)+1):d^2])
   V.eps.inv.2= diag(1/p[(d^2+1):(d*(d+1))])

   M=matrix(0,nrow = 23,ncol = 23)
   M[1,] = c(1,-p[1:22])
   M[23,] = c(-p[(d^2-d-21):(d^2-d)],1)
   for(i in 2:22)
      M[i,] = -c(p[(23*(i-1)+1):(23*(i-1)+i-1)],-1,p[(23*(i-1)+i):(23*i-1)])
   
    RVY1= cov(districts.cleaned[1:300,])
    RVY2= cov(districts.cleaned[301:554,])
   
    Y1=t(M) %*% V.eps.inv.1 %*% M - solve(RVY1)
    Y2=t(M) %*% V.eps.inv.2 %*% M - solve(RVY2)
    
    upper.Y1 <- Y1
   
    for(i in 1:d){
       for(j in 1:d){
          if(i > j) upper.Y1[i,j] <- NA
       }
    }
    upper.Y2 <- Y2
    
    for(i in 1:d){
       for(j in 1:d){
          if(i > j) upper.Y2[i,j] <- NA
       }
    }
   
    Z1 <- upper.Y1 %>% t() %>% as.numeric()
    Z1 <- Z1[is.na(Z1) == FALSE]
    Z2 <- upper.Y2 %>% t() %>% as.numeric()
    Z2 <- Z2[is.na(Z2) == FALSE]
   
    return(c(Z1,Z2))
    
}
x=c(rep(0.01,d^2-d),rep(10,2*d))

nleqslv(x, f, control=list(btol=.01),jacobian=TRUE,method="Newton")













