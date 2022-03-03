library(dplyr)
library(expm)

# districts.df <- read.csv(file.choose(),header = TRUE)
# districts.df <- as.data.frame(districts.df)

path <- "D:/My Documents/R/R Codes/Project on Spread of COVID-19/Datasets/"
path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"

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

# V2 <- diag(diag(RVY2.inv))
#E1 <- eigen(RVY1)

E2 <- eigen(RVY2.inv)
E2.val <- E2$values
E2.vec <- E2$vectors %>% t()

temp2 <- sqrtm(diag(E2.val)) %*% E2.vec
V2 <- diag(diag(temp2^2))

M <- solve(sqrtm(V2)) %*% temp2

target <- which(diag(M) < 0)
M[target, ] <- -M[target, ]

inv.M <- solve(M)

V1 <- t(inv.M) %*% RVY1.inv %*% inv.M
   





