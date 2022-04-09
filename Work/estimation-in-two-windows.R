library(dplyr)
library(expm)
path <- paste0(getwd(),"/Datasets/")

districts.df <- read.csv(paste0(path,"districts.csv")) %>% as.data.frame()
districts.conf <- districts.df %>% filter(State == "West Bengal")
uni.dist <- unique(districts.conf$District)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other State" , ]
uni <- uni[,c(1,3,4)]

uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))

for (i in 1:length(uni.dist)) {
   X[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

N <- 554
M <- matrix(0, nrow = N, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, N-length(X[[k]])), X[[k]])
}

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
}) %>% 
   apply(c(1,2), function(val){
      if(val < 0){
         return(0)
      }
      else return(log(val + 1))
   }) %>% as.data.frame()

uni.dist <- uni.dist %>% gsub(" ", ".", .)
colnames(districts.cleaned) <- uni.dist

arranged.dist <- c("Alipurduar","Cooch.Behar","Jalpaiguri",
                   "Kalimpong","Darjeeling","Uttar.Dinajpur",
                   "Dakshin.Dinajpur","Malda","Murshidabad",
                   "Birbhum","Purba.Bardhaman","Nadia",
                   "Paschim.Bardhaman","Bankura","Hooghly",
                   "North.24.Parganas","Purulia","Jhargram",
                   "Paschim.Medinipur","Howrah","Kolkata",
                   "South.24.Parganas","Purba.Medinipur")

ord <- match(arranged.dist, uni.dist)

lst <- list()
for(i in 1:23){
   lst[[i]] <- districts.cleaned[,ord[i]]
}

districts.cleaned.arranged <- do.call(cbind,lst)
colnames(districts.cleaned.arranged) <- arranged.dist

temp3 <- as.vector(as.matrix(districts.cleaned.arranged))

temp.ts <- data.frame(day = rep(c(1:N), times = 23),
                      district = rep(arranged.districts, each = N),
                      data = temp3)


library(dplyr)
library(vars)
library(readxl)

# n <- 7

ts.mat <-function(n,start,end){
   if(TRUE){
      path <- paste0(getwd(),"/Datasets/")
      
      districts.df <- read.csv(paste0(path,"districts.csv")) %>% as.data.frame()
      districts.conf <- districts.df %>% filter(State == "West Bengal")
      districts.conf[,3] <- districts.conf[,3] %>% gsub(" ", ".", .)
      
      arranged.districts.data <- read_excel(paste0(path,"arranged_districts.xlsx"))[,-1]
      arranged.districts <- colnames(arranged.districts.data)
      
      uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                               districts.conf[,3] != "Other.State" , ][,c(1,3,4)]
      
      # uni.dist <- districts.conf$District %>%
      #    unique() %>%
      #    setdiff(c("Unknown", "Other.State"))
      
      uni.dist <- arranged.districts
      L <- uni.dist %>% length()
      
      X <- list()
      for (i in 1:length(uni.dist)) {
         X[[i]] <- uni[uni[,2] == uni.dist[i],3]
      }
      
      J <- districts.conf[,1] %>% unique() %>% length()
      M <- matrix(0, nrow = J, ncol = L)
      
      for (k in 1:L) {
         M[,k] <- c(rep(0, nrow(M)-length(X[[k]])), X[[k]])
      }
      
      districts.cleaned <- apply(M, 2, function(vec){
         temp <- c(0, vec[-length(vec)])
         return(vec - temp)
      })
      
      districts.cleaned <- districts.cleaned %>% as.data.frame()
      districts.cleaned = districts.cleaned[start:end,]
      colnames(districts.cleaned) <- uni.dist
      
      #E <- districts.cleaned %>% select(arranged.districts)
   }
   districts.cleaned <- districts.cleaned %>% apply(c(1,2), function(val){
      if(val < 0){
         return(0)
      }
      else return(log(val + 1))
   }) %>% as.data.frame()
   
   G <- matrix(0, nrow = floor((end-start+1)/n), ncol = L)
   
   for(i in 1:L){
      for(k in 1:floor((end-start+1)/n)){
         # G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])/n
         G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])
      }
   }
   
   G <- G %>% as.data.frame()
   colnames(G) <- uni.dist
   rownames(G) <- sapply(1:floor((end-start+1)/n), function(val){paste0("w",val)})
   
   x <- VAR(G, p = 2, type = "const")
   S <- x %>% summary()
   
   A_1 <- A_2 <- matrix(0, nrow = L, ncol = L)
   ptol <- 0.1
   
   for(i in 1:L){
      temp1 <- S$varresult[[i]]$coefficients[1:L , ]
      temp2 <- S$varresult[[i]]$coefficients[(L+1):(2*L) , ]
      
      mark1 <- which(temp1[,4] < ptol) %>% as.numeric()
      mark2 <- which(temp2[,4] < ptol) %>% as.numeric()
      
      A_1[i , mark1] <- temp1[mark1 , 1]
      A_2[i , mark2] <- temp2[mark2 , 1]
   }
   
   colnames(A_1) <- colnames(A_2) <- rownames(A_1) <- rownames(A_2) <- uni.dist
   return(list("B_1" = A_1, "B_2" = A_2, "G" = G))
}

temp <- ts.mat(1,1,300)
temp1 <- ts.mat(1,301,554)

G <- as.matrix(temp$G)
residual.1.w1 <- G[3:150,] - G[2:149,] %*% temp$B_1 - G[1:148,] %*% temp$B_2
residual.2.w1 <- G[151:nrow(G),] - G[150:(nrow(G)-1),] %*% temp$B_1 - G[149:(nrow(G)-2),] %*% temp$B_2

RVY1.inv.w1 <- solve(cov(residual.1.w1))
RVY2.inv.w1 <- solve(cov(residual.2.w1))

G1 <- as.matrix(temp1$G)
residual.1.w2 <- G1[3:120,] - G1[2:119,] %*% temp1$B_1 - G1[1:118,] %*% temp1$B_2
residual.2.w2 <- G1[121:nrow(G1),] - G1[120:(nrow(G1)-1),] %*% temp1$B_1 - G1[119:(nrow(G1)-2),] %*% temp1$B_2

RVY1.inv.w2 <- solve(cov(residual.1.w2))
RVY2.inv.w2 <- solve(cov(residual.2.w2))


E2 <- eigen(RVY2.inv.w1)   # C2 = RVY2.inv
E2.val <- E2$values
E2.vec <- E2$vectors %>% t()

temp2 <- sqrtm(diag(E2.val)) %*% E2.vec
V2.w1 <- diag(diag(temp2^2))

M.w1 <- solve(sqrtm(V2.w1)) %*% temp2

target <- which(diag(M.w1) < 0)
M.w1[target, ] <- -M.w1[target, ]

inv.M.w1 <- solve(M.w1)

V1.w1 <- t(inv.M.w1) %*% RVY1.inv.w1 %*% inv.M.w1

E2 <- eigen(RVY2.inv.w2)   # C2 = RVY2.inv
E2.val <- E2$values
E2.vec <- E2$vectors %>% t()

temp2 <- sqrtm(diag(E2.val)) %*% E2.vec
V2.w2 <- diag(diag(temp2^2))

M.w2 <- solve(sqrtm(V2.w2)) %*% temp2

target <- which(diag(M.w2) < 0)
M.w2[target, ] <- -M.w2[target, ]

inv.M.w2 <- solve(M.w2)

V1.w2 <- t(inv.M.w2) %*% RVY1.inv.w2 %*% inv.M.w2

W.w1=diag(rep(1,23))- M.w1
W.w2=diag(rep(1,23))- M.w2
