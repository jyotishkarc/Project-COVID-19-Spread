
library(dplyr)
library(vars)
library(readxl)

n <- 7
ts.mat <-function(n){
   if(TRUE){
      path <- "D:/My Documents/R/R Codes/Project on Spread of COVID-19/Datasets/"
      #path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"
      
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
      colnames(districts.cleaned) <- uni.dist
      
      #E <- districts.cleaned %>% select(arranged.districts)
   }
   
   G <- matrix(0, nrow = floor(J/n), ncol = L)
   
   for(i in 1:L){
      for(k in 1:floor(J/n)){
         # G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])/n
         G[k,i] <- sum(districts.cleaned[(n*k-n+1):(n*k),i])
      }
   }
   
   G <- G %>% as.data.frame()
   colnames(G) <- uni.dist
   rownames(G) <- sapply(1:floor(J/n), function(val){paste0("w",val)})
   
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
   N <- as.data.frame(rep(NA,23))
   return(cbind(A_1,N,A_2))
}

matrix.list <- matrix.list.all <- list()
current.sheet.name <- c()

for (k in 1:7) {
   matrix.list[[k]] <- as.data.frame(ts.mat(k))
   current.sheet.name[k] <- paste0(k," - ")
   print(current.sheet.name[k])
   matrix.list.all <- append(matrix.list.all,
                                 list(matrix.list[[k]]))
}

names(matrix.list.all) <- current.sheet.name

writexl::write_xlsx(matrix.list.all, path ="C:\\Users\\JYOTISHKA\\Desktop\\matrix.list.all.xlsx")

#writexl::write_xlsx(matrix.list.all, path ="/Users/aytijhyasaha/Desktop/matrix.list.all.xlsx")


