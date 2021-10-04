library(dplyr)
districts.df <-read.csv(file.choose(),header = TRUE)
districts.df <- as.data.frame(districts.df)
districts.conf <- districts.df %>% filter(State == "West Bengal")

uni.dist <- unique(districts.conf$District)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other State" , ]
uni <- uni[,c(1,3,4)]

uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))
X=list()
for (i in 1:length(uni.dist)) {
   X[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

M <- matrix(0, nrow = 503, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, nrow(M)-length(X[[k]])), X[[k]])
}
#colnames(M)=uni.dist

reg.list <- list()

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

districts.cleaned <- as.data.frame(districts.cleaned)

Y=districts.cleaned <- districts.cleaned[151:180,]
colnames(Y) <- uni.dist




mat=list()
for(i in 1:23){
   v <- cor(Y[,i],Y[,-i])
   reg <- matrix(0, ncol = 22, nrow = 1) %>% as.data.frame
   colnames(reg)[1] <- colnames(v)[which.max(abs(v))]
   reg[,1] <- max(abs(v))
   
   
   for(k in 2:22){
      a <- matrix(0, ncol = 23, nrow = 1) %>% as.data.frame
      colnames(a) <- uni.dist
      for(j in 1:23){
         
         if(j != i & !(colnames(Y)[j] %in% colnames(reg))){
            a[j] <- pcor.test(Y[,i],Y[,j],
                              Y[,which(colnames(Y) %in% colnames(reg))])$estimate
         }
      }
      cat("a")
      print(a)
      colnames(reg)[k] = colnames(a)[which.max(abs(a))]
      reg[,k] <- max(abs(a))
      
   }
   mat[[i]]=reg
   print(reg)
}