library(dplyr)
library(expm)

path <- "D:/My Documents/R/R Codes/Project on Spread of COVID-19/Datasets/"
path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"

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

as_tibble(temp.ts) %>% 
   filter(district == arranged.districts[1:8]) %>% 
   plot_time_series(day, data,
                    .interactive = FALSE,
                    .facet_vars = district,
                    .facet_ncol = 2,
                    .facet_scales = "fixed",
                    # .color_var = district,
                    .legend_show = FALSE,
                    .title = "")

as_tibble(temp.ts) %>% 
   filter(district == arranged.districts[9:16]) %>% 
   plot_time_series(day, data,
                    .interactive = FALSE,
                    .facet_vars = district,
                    .facet_ncol = 2,
                    .facet_scales = "fixed",
                    # .color_var = district,
                    .legend_show = FALSE,
                    .title = "")

as_tibble(temp.ts) %>% 
   filter(district == arranged.districts[17:23]) %>% 
   plot_time_series(day, data,
                    .interactive = FALSE,
                    .facet_vars = district,
                    .facet_ncol = 2,
                    .facet_scales = "fixed",
                    # .color_var = district,
                    .legend_show = FALSE,
                    .title = "")

x <- VAR(districts.cleaned.arranged, p = 2, type = "const")
S <- x %>% summary()

B_1 <- B_2 <- matrix(0, nrow = L, ncol = L)
ptol <- 0.1

for(i in 1:L){
   temp1 <- S$varresult[[i]]$coefficients[1:L , ]
   temp2 <- S$varresult[[i]]$coefficients[(L+1):(2*L) , ]
   
   mark1 <- which(temp1[,4] < ptol) %>% as.numeric()
   mark2 <- which(temp2[,4] < ptol) %>% as.numeric()
   
   B_1[i , mark1] <- temp1[mark1 , 1]
   B_2[i , mark2] <- temp2[mark2 , 1]
}

colnames(B_1) <- colnames(B_2) <- 
   rownames(B_1) <- rownames(B_2) <- arranged.dist

predicted <- rbind(districts.cleaned.arranged[1:400, ],
                   matrix(0, nrow = (N-400), ncol = 23))
const=c()
for(i in 1:23)
   const[i]=x[["varresult"]][[arranged.dist[i]]][["coefficients"]][["const"]]
for(day in 401:N){
   predicted[day, ] <- B_1 %*% predicted[(day - 1), ] +
                        B_2 %*% predicted[(day - 2), ] +
                        const
                        
}

p=list()
for(i in 1:23){
   p[[i]] = ggplot() + 
      geom_line(data = as.data.frame(cbind(x1 = 1:554,
                                           y1 = districts.cleaned.arranged[,i])),
                aes(x = x1, y = y1), color = "blue") +
      geom_line(data = as.data.frame(cbind(x2 = 401:554,
                                           y2 = predicted[401:554,i])), 
                aes(x = x2, y = y2), color = "red") +
      xlab('Day') +
      ylab('Infection') +
      ggtitle(arranged.dist[i])
} 

