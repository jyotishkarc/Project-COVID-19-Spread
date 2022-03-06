library(dplyr)
path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"
# districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")
districts.df <- read.csv(paste0(path,"districts.csv")) %>% as.data.frame()
districts.df <- as.data.frame(districts.df)
districts.conf <- districts.df %>% filter(State == "West Bengal")

uni.dist <- unique(districts.conf$District)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other State" , ][,c(1,3,4)]
# uni <- uni[,c(1,3,4)]

uni.dist <- setdiff(uni.dist, c("Unknown", "Other State"))

H <- list()

for (i in 1:length(uni.dist)) {
   H[[i]] <- uni[uni[,2] == uni.dist[i],3]
}

M <- matrix(0, nrow = 554, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, nrow(M)-length(H[[k]])), H[[k]])
}


M.v1 <- M[-c(1:13), ]

districts.cleaned <- apply(M.v1, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
})

districts.cleaned <- as.data.frame(districts.cleaned)

uni.dist <- uni.dist %>% gsub(" ", ".", .)
colnames(districts.cleaned)=uni.dist
arranged.dist=c("Alipurduar","Cooch.Behar","Jalpaiguri","Kalimpong","Darjeeling","Uttar.Dinajpur","Dakshin.Dinajpur","Malda","Murshidabad","Birbhum","Purba.Bardhaman","Nadia","Paschim.Bardhaman","Bankura","Hooghly","North.24.Parganas","Purulia","Jhargram","Paschim.Medinipur","Howrah","Kolkata","South.24.Parganas","Purba.Medinipur"
)
ord=c()
for(i in 1:23)
   ord[i]=match(arranged.dist[i],uni.dist)
lst=list()
for(i in 1:23)
   lst[[i]]=districts.cleaned[,ord[i]]
districts.cleaned.arranged=do.call(cbind,lst)
colnames(districts.cleaned.arranged)=arranged.dist
########## Function for estimating the W matrix ##########

reg.matrix <- function(start.day, end.day){
   X <- as.data.frame(districts.cleaned.arranged)[c(start.day : end.day),]
   V <- as.data.frame(cov(X))
   pref <- W <- matrix(0, nrow = 23, ncol = 23) %>% as.data.frame()
   
   colnames(X) <- colnames(V) <- rownames(V) <- 
      colnames(pref) <- rownames(pref) <- colnames(W) <- rownames(W) <- uni.dist
   
   
   for (i in 1:ncol(X)) {
      for (j in 1:ncol(X)) {
         
         if (j == i) {next}
         
         V.11 <- V[c(i,j),c(i,j)]
         V.12 <- V[c(i,j),-c(i,j)]
         #V.21 <- t(V12)
         V.22 <- V[-c(i,j),-c(i,j)]
         
         V.11.2 <- V.11 - as.matrix(V.12) %*% solve(V.22) %*% t(V.12)
         
         if(abs(V.11.2[1,2] / V.11.2[1,1]) > abs(V.11.2[1,2] / V.11.2[2,2])){
            pref[j,i] <- 1
         }
         else pref[i,j] <- 1
      }
   }
   
   reg.list <- list()
   
   
   for (k in 1 : ncol(X)) {
      cat("\n")
      print(k)
      cat("\n")
      
      temp.M <- X[ ,c(k, which(pref[k,] == 1))] %>% as.data.frame()
      N <- ncol(temp.M)
      
      names(temp.M)[1] <- "current.dep.var"
      reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
      Q <- reg.list[[k]] %>% summary()
      p <- Q$coefficients[,4][-1]
      y <- 0
      
      if(max(p) < 0.05){
         temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
         s <- rownames(Q$coefficients)[-1]
         W[k,s] <- Q$coefficients[,1][-1]
         print(reg.list[[k]] %>% summary())
      }
      else{
         while (max(p) >= 0.05 & y < (N - 2)) {
            y <- y+1
            s <- rownames(Q$coefficients)[1+which.max(p)] %>% gsub("`","",.)
            temp.M <- temp.M[,-which(names(temp.M) %in% s)]
            print(dim(temp.M))
            reg.list[[k]] <- lm(current.dep.var ~ . , data = temp.M)
            Q <- reg.list[[k]] %>% summary()
            p <- Q$coefficients[,4][-1]
            
            print(max(p))
            
            if(max(p) < 0.05){
               temp <- as.numeric(reg.list[[k]]$coefficients)[-1]
               s <- rownames(Q$coefficients)[-1]
               W[k,s] <- Q$coefficients[,1][-1]
               print(reg.list[[k]] %>% summary())
               break
            }
         }
      }
   }
   
   
   
   W[is.na(W)] <- 0
   
   print("DONE")
   
   # return(W)
   
   return(cbind(" " = arranged.dist, W))
}

##########################################################

start.day <- seq(41,461, by = 30)
end.day <- start.day + 59

reg.matrix.list <- reg.matrix.list.all <- list()

for (k in 1:length(start.day)) {
   
   cat("\n",k,"\n")
   
   reg.matrix.list[[k]] <- reg.matrix(start.day = start.day[k], 
                                      end.day = end.day[k])
   
   current.sheet.name[k] <- paste0(start.day[k]," - ",end.day[k])
   print(current.sheet.name[k])
   
   reg.matrix.list.all <- append(reg.matrix.list.all,
                                 list(reg.matrix.list[[k]]))
}

names(reg.matrix.list.all) <- current.sheet.name

writexl::write_xlsx(reg.matrix.list.all, 
                    path = "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/reg-matrix-all-arranged.xlsx")








