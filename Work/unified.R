library(dplyr)
library(expm)
library(timetk)
library(vars)
library(ggplot2)
library(forecast)
library(ggpubr)

## source('~/R/R Codes/Project on Spread of COVID-19/preamble.R')

##### Usual Time Series Plot
if(FALSE){
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
}
############################

last.train <- 520

x <- VAR(districts.cleaned.arranged[1:last.train, ], p = 2, type = "const")
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

predicted <- rbind(districts.cleaned.arranged[1:last.train, ],
                   matrix(0, nrow = (N-last.train), ncol = 23))
# const=c()
# for(i in 1:23)
#    const[i]=x[["varresult"]][[arranged.dist[i]]][["coefficients"]][["const"]]
# for(day in (last.train + 1):N){
#    predicted[day, ] <- B_1 %*% predicted[(day - 1), ] +
#                         B_2 %*% predicted[(day - 2), ] +
#                         const
#                         
# }
y=predict(x, n.ahead = 554 - last.train)
for(day in (last.train + 1):N){
   
   for(j in 1:23)
      predicted[day,j ] = y$fcst[[j]][day-last.train,1]
   
}
p=list()
for(i in 1:23){
   p[[i]] = ggplot() + 
      geom_line(data = as.data.frame(cbind(x1 = 1:554,
                                           y1 = districts.cleaned.arranged[,i])),
                aes(x = x1, y = y1), color = "blue") +
      geom_line(data = as.data.frame(cbind(x2 = (last.train +1):554,
                                          y2 = predicted[(last.train +1):554,i])), 
                aes(x = x2, y = y2), color = "red") +
      xlab('Day') + ylab('Infection') +
      ggtitle(arranged.dist[i]) + theme(
         plot.title = element_text(size=12, face="bold", hjust = 0.5),
      )
} 


pred1 <- ggarrange(p[[1]], p[[2]], p[[3]], p[[4]],
          p[[5]], p[[6]], p[[7]], p[[8]], 
          labels = NULL,
          ncol = 2, nrow = 4)


pred2 <- ggarrange(p[[9]], p[[10]], p[[11]], p[[12]],
                p[[13]], p[[14]], p[[15]], p[[16]], 
                labels = NULL,
                ncol = 2, nrow = 4)

pred3 <- ggarrange(p[[17]], p[[18]], p[[19]], p[[20]],
                   p[[21]], p[[22]], p[[23]], 
                   labels = NULL,
                   ncol = 2, nrow = 4)



