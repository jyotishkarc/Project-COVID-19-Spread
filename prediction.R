library(dplyr)
library(vars)
library(readxl)
path <- "D:/My Documents/R/R Codes/Project on Spread of COVID-19/Datasets/"
path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"

districts.df <- read.csv(paste0(path,"districts.csv")) %>% as.data.frame()
districts.conf <- districts.df %>% filter(State == "West Bengal")
districts.conf[,3] <- districts.conf[,3] %>% gsub(" ", ".", .)

arranged.districts.data <- read_excel(paste0(path,"arranged_districts.xlsx"))[,-1]
arranged.districts <- colnames(arranged.districts.data)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other.State" , ][,c(1,3,4)]

uni.dist <- uni$District %>%    unique()
uni.dist=uni.dist[-c(14,17)]
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
colnames(districts.cleaned) <- uni.dist

x=VAR(districts.cleaned[1:400,], p = 2, type = "const")
L = 23
B_1 <- B_2 <- matrix(0, nrow = L, ncol = L)
for(i in 1:L){
   B_1[i , ] <- x$varresult[[i]]$coefficients[1:L ]
   B_2[i , ] <- x$varresult[[i]]$coefficients[(L+1):(2*L) ]
}
colnames(B_1) <- colnames(B_2) <- rownames(B_1) <- rownames(B_2) <- uni.dist
var.pred = predict(x, n.ahead = nrow(districts.cleaned)-400)
error=numeric(L)

for (i in 1:L) 
   for (j in 401:554) 
      error[i]=error[i]+abs(var.pred$fcst[i][[1]][j-400,1]-districts.cleaned[j,i])/districts.cleaned[j,i]

error=error/154
names(error)=uni.dist 
error

const=numeric(L)
for(i in 1:L){
   const[i]=x$varresult[[i]]$coefficients[47]
}
B_1 %*% districts.cleaned[400,]+B_2 %*% districts.cleaned[399,]->y
