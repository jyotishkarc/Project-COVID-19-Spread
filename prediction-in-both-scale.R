library(dplyr)
library(expm)
library(vars)

# path.preamble <- paste0(getwd(),"/preamble.R")
# source(path.preamble)

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
