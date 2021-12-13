library(dplyr)
#stepwise regression
#abcd
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

districts.cleaned <- districts.cleaned[151:180,]

# districts.cleaned.centred <- districts.cleaned -
#    matrix(rep(colMeans(districts.cleaned), nrow(M)), nrow(M), ncol(M), byrow = TRUE)

W <-as.data.frame(matrix(0, nrow = ncol(M), ncol = ncol(M)))


for (k in 1 : ncol(districts.cleaned)) {
   temp.M <- districts.cleaned
   # temp.M <- districts.cleaned.centred
   print(names(temp.M)[k])
   names(temp.M)[k] <- "current.dep.var"
   reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
   Q <- reg.list[[k]] %>% summary()
   p <- Q$coefficients[,4][-1]
   y=0
   if(max(p) < 0.05){
      temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
      s=rownames(Q$coefficients)[-1]
      W[k,s] <- Q$coefficients[,1][-1]
      print(reg.list[[k]] %>% summary())
   }
   else{
      while (max(p)>=0.05 & y< 21) {
         y=y+1
         s=rownames(Q$coefficients)[1+which.max(p)]
         temp.M=temp.M[,-which(names(temp.M) %in% s)]
         reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
         Q <- reg.list[[k]] %>% summary()
         p <- Q$coefficients[,4][-1]
         if(max(p) < 0.05){
            temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
            s=rownames(Q$coefficients)[-1]
            W[k,s] <- Q$coefficients[,1][-1]
            print(reg.list[[k]] %>% summary())
            
         }
      }
   }
   
}
colnames(W) <- uni.dist -> rownames(W)
library(writexl)
write_xlsx(W,"C:\\Users\\Aytijhya Saha\\OneDrive\\Desktop\\projects\\Spread of covid 19 project\\step_regression.xlsx")
