path <- "/Users/aytijhyasaha/Desktop/projects/spread of covid/Project-COVID-19-Spread/Datasets/"
#districts.df <- read.csv("G:/B.Stat/Projects/Gopal K. Basak/GKB Sir/Dataset/districts.csv")
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

M <- matrix(0, nrow = 541, ncol = 23)

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
colnames(districts.cleaned) <- uni.dist
arranged.dist=c("Alipurduar","Cooch.Behar","Jalpaiguri",
                "Kalimpong","Darjeeling","Uttar.Dinajpur",
                "Dakshin.Dinajpur","Malda","Murshidabad",
                "Birbhum","Purba.Bardhaman","Nadia",
                "Paschim.Bardhaman","Bankura","Hooghly",
                "North.24.Parganas","Purulia","Jhargram",
                "Paschim.Medinipur","Howrah","Kolkata",
                "South.24.Parganas","Purba.Medinipur")
# ord=c()
# for(i in 1:23)
#    ord[i]=match(arranged.dist[i],uni.dist)

ord <- match(arranged.dist, uni.dist)

lst=list()
for(i in 1:23)
   lst[[i]]=districts.cleaned[,ord[i]]
districts.cleaned.arranged=do.call(cbind,lst)
colnames(districts.cleaned.arranged)=arranged.dist

# x1=ts(districts.cleaned[,1:7]) #this makes sure R knows that x is a time series
# plot(x1)
# x2=ts(districts.cleaned[,8:15]) #this makes sure R knows that x is a time series
# plot(x2)
# x3=ts(districts.cleaned[,16:23]) #this makes sure R knows that x is a time series
# plot(x3)
# 
# v1=cov(districts.cleaned[1:50,])
# v2=cov(districts.cleaned[51:250,])
# v3=cov(districts.cleaned[251:350,])
# v4=cov(districts.cleaned[351:450,])
# v5=cov(districts.cleaned[451:554,])


temp2 <- as.vector(as.matrix(districts.cleaned))

temp.ts <- data.frame(day = rep(c(1:541), times = 23),
                     district = rep(arranged.districts, each = 541),
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

# for(i in 1:23){
#    p = ggplot() + 
#       geom_line(data = as.data.frame(cbind(x1 = 1:554,y1 = districts.cleaned.arranged[,i])), aes(x = x1, y = y1), color = "blue") +
#       geom_line(data = as.data.frame(cbind(x2 = 401:554,y2 = predicted)), aes(x = x2, y = y2), color = "blue") +
#       xlab('Dates') +
#       ylab('percent.change')
# }   
