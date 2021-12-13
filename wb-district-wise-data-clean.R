# districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")

districts.df <- as.data.frame(districts.df)
districts.conf <- districts.df %>% filter(State == "West Bengal")

districts.conf[,3] <- districts.conf[,3] %>% gsub(" ", ".", .)

uni <- districts.conf[districts.conf[,3] != "Unknown" ||
                         districts.conf[,3] != "Other.State" , ][,c(1,3,4)]

uni.dist <- districts.conf$District %>%
               unique() %>% 
               setdiff(c("Unknown", "Other.State"))

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

districts.cleaned <- as.data.frame(districts.cleaned)
colnames(districts.cleaned) <- uni.dist

# districts.cleaned <- districts.cleaned[151:180,]

arranged.districts.data <- read_excel("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/arranged_districts.xlsx")[,-1]

arranged.districts <- colnames(arranged.districts.data)












