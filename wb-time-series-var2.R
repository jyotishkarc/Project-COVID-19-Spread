
library(dplyr)
library("vars")

n <- 7

districts.conf <- districts.df %>% filter(State == "West Bengal")

districts.conf[,3] <- districts.conf[,3] %>% gsub(" ", ".", .)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other.State" , ][,c(1,3,4)]

uni.dist <- districts.conf$District %>%
   unique() %>% 
   setdiff(c("Unknown", "Other.State"))
# gsub(" ", ".", .)

X <- list()
for (i in 1:length(uni.dist)) {
   X[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

J <- districts.conf[,1] %>% unique() %>% length()
M <- matrix(0, nrow = J, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, nrow(M)-length(X[[k]])), X[[k]])
}

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})



G <- matrix(0, nrow = floor(J/n), ncol = 23)

for(i in 1:23){
   for(k in 1:floor(J/n)){
      G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])/n
   }
}

G <- as.data.frame(G)
colnames(G) <- uni.dist
rownames(G) <- sapply(1:floor(J/n), function(val){paste0("w",val)})
x= VAR(G, p=2, type="both")
summary(x)