
library(dplyr)
library(vars)
library(readxl)

n <- 7

if(TRUE){
   
   path <- "D:/My Documents/R/R Codes/Project on Spread of COVID-19/Datasets/"
   # path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"
   
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
   
   districts.cleaned <- districts.cleaned %>% as.data.frame()
   colnames(districts.cleaned) <- uni.dist
   
   #E <- districts.cleaned %>% select(arranged.districts)
}


G <- matrix(0, nrow = floor(J/n), ncol = 23)

for(i in 1:23){
   for(k in 1:floor(J/n)){
      # G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])/n
      G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])
   }
}

G <- G %>% as.data.frame()
colnames(G) <- uni.dist
rownames(G) <- sapply(1:floor(J/n), function(val){paste0("w",val)})

x <- VAR(G, p = 2, type = "both")
S <- x %>% summary()

S$varresult[[23]]$coefficients






