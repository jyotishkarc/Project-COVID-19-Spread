library(dplyr)
library(expm)
library(vars)
# districts.df <- read.csv(file.choose(),header = TRUE)
# districts.df <- as.data.frame(districts.df)

path <- "D:/My Documents/R/R Codes/Project on Spread of COVID-19/Datasets/"
# path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"

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

districts.cleaned <- districts.cleaned %>% apply(c(1,2), function(val){
   if(val < 0){
      return(0)
   }
   else return(log(val + 1))
}) %>% as.data.frame()

N=500

x <- VAR(districts.cleaned[1:N,], p = 2, type = "const")
L <- 23
B_1 <- B_2 <- matrix(0, nrow = L, ncol = L)

for(i in 1:L){
   B_1[i,] <- x$varresult[[i]]$coefficients[1:L]
   B_2[i,] <- x$varresult[[i]]$coefficients[(L+1):(2*L)]
}

colnames(B_1) <- colnames(B_2) <- rownames(B_1) <- rownames(B_2) <- uni.dist


var.pred <- predict(x, n.ahead = nrow(districts.cleaned)-N)
var.pred.usual <- lapply(var.pred$fcst, function(lst){
   apply(lst, c(1,2), function(val){
      return(exp(val) - 1)
   })
})

error <- error.usual <- numeric(L)

for (i in 1:L) 
   for (j in (N+1):554) 
      error[i]=error[i]+abs(var.pred$fcst[i][[1]][j-N,1]-districts.cleaned[j,i])

for (i in 1:L) 
   for (j in (N+1):554) 
      error.usual[i]=error.usual[i]+abs(var.pred.usual[i][[1]][j-N,1]-exp(districts.cleaned[j,i]) -1)/(exp(districts.cleaned[j,i]) - 1)

error=error/54
error.usual=error.usual/54
names(error) <- names(error.usual) <- uni.dist 
error
