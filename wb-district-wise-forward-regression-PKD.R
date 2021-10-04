library(dplyr)
library(ppcor)
districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")

districts.df <- as.data.frame(districts.df)
districts.conf <- districts.df %>% filter(State == "West Bengal")

uni.dist <- unique(districts.conf$District)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other State" , ]
uni <- uni[,c(1,3,4)]

uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))

X <- list()
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

Y <- districts.cleaned <- districts.cleaned[151:180,]
colnames(Y) <- uni.dist


for(i in 1:23){
   v <- cor(Y[,i],Y[,-i])
   reg <- matrix(0, ncol = 22, nrow = 1) %>% as.data.frame
   colnames(reg)[1] <- colnames(v)[which.max(abs(v))]
   reg[,1] <- max(abs(v))
   
   m <- 2
   
   for(j in 1:23){
      a <- rep(0,23)
      
      if(j != i & !(colnames(Y)[j] %in% colnames(reg))){
         a[j] <- pcor.test(Y[,i],Y[,j],
                           Y[,which(colnames(Y) %in% colnames(reg))])$estimate
      }
         
      if (m != 23) {
         colnames(reg)[m] <- setdiff(colnames(v), colnames(reg))[which.max(abs(a))]
         reg[,m] <- max(abs(a))
      }
      
      j <- j + 1
      m <- m + 1
   }











