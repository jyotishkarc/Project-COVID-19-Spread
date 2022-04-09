
library("raster")

ind2 <- getData('GADM', country='IND', level=2)
wb2 <- ind2[ind2$NAME_1=="West Bengal",]
cities <- data.frame(name="Purulia", lon=86.36521, lat=23.33208)

# plot
plot(wb2, border='gray', col='light gray')
points(cities[, 2:3], col='red', pch=20)
text(cities[, 2:3], labels= cities[,1], pos=4)

