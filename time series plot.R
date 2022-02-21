

x1=ts(districts.cleaned[,1:7]) #this makes sure R knows that x is a time series
plot(x1)
x2=ts(districts.cleaned[,8:15]) #this makes sure R knows that x is a time series
plot(x2)
x3=ts(districts.cleaned[,16:23]) #this makes sure R knows that x is a time series
plot(x3)

v1=cov(districts.cleaned[1:50,])
v2=cov(districts.cleaned[51:250,])
v3=cov(districts.cleaned[251:350,])
v4=cov(districts.cleaned[351:450,])
v5=cov(districts.cleaned[451:554,])


