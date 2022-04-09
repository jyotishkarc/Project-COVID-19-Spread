library(dplyr)

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

N <- 554
M <- matrix(0, nrow = N, ncol = 23)

for (k in 1:23) {
   M[,k] <- c(rep(0, N-length(X[[k]])), X[[k]])
}

districts.cleaned <- apply(M, 2, function(vec){
   temp <- c(0, vec[-length(vec)])
   return(vec - temp)
}) %>% 
   apply(c(1,2), function(val){
      if(val < 0){
         return(0)
      }
      else return(log(val + 1))
   }) %>% as.data.frame()

uni.dist <- uni.dist %>% gsub(" ", ".", .)
colnames(districts.cleaned) <- uni.dist
arranged.dist <- c("Alipurduar","Cooch.Behar","Jalpaiguri",
                   "Kalimpong","Darjeeling","Uttar.Dinajpur",
                   "Dakshin.Dinajpur","Malda","Murshidabad",
                   "Birbhum","Purba.Bardhaman","Nadia",
                   "Paschim.Bardhaman","Bankura","Hooghly",
                   "North.24.Parganas","Purulia","Jhargram",
                   "Paschim.Medinipur","Howrah","Kolkata",
                   "South.24.Parganas","Purba.Medinipur")

ord <- match(arranged.dist, uni.dist)

lst <- list()
for(i in 1:23){
   lst[[i]] <- districts.cleaned[,ord[i]]
}

districts.cleaned.arranged <- do.call(cbind,lst)
colnames(districts.cleaned.arranged) <- arranged.dist

temp2 <- as.vector(as.matrix(districts.cleaned))

temp.ts <- data.frame(day = rep(c(1:N), times = 23),
                      district = rep(arranged.districts, each = N),
                      data = temp2)

