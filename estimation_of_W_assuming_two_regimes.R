f <- function(p){
   d <- 23
   V.eps.inv.1= diag(1/p[(d*(d-1)+1):d^2])
   V.eps.inv.2= diag(1/p[(d^2+1):(d*(d+1))])
   print(dim(V.eps.inv.1))
   print(dim(V.eps.inv.2))
   M=matrix(0,nrow = 23,ncol = 23)
   M[1,] = c(1,-p[1:22])
   M[23,] = c(-p[(d^2-d-21):(d^2-d)],1)
   for(i in 2:22)
      M[i,] = -c(p[(23*(i-1)+1):(23*(i-1)+i-1)],-1,p[(23*(i-1)+i):(23*i-1)])
   
    RVY1= cov(districts.cleaned[1:300,])
    RVY2= cov(districts.cleaned[301:554,])
    Y1=t(M) %*% V.eps.inv.1 %*% M - solve(RVY1)
    Y2=t(M) %*% V.eps.inv.2 %*% M - solve(RVY2)
    
    upper.Y1 <- Y1
    
    for(i in 1:d){
       for(j in 1:d){
          if(i >= j) upper.Y1[i,j] <- NA
       }
    }
    
    Z <- upper.Y1 %>% t() %>% as.numeric()
    Z <- Z[is.na(Z) == FALSE]
    
    return(Z)
    
}
x=rep(1,length(d^2+d))

nleqslv(x, f, control=list(btol=.01),jacobian=TRUE,method="Newton")














# V.eps.inv.1= diag(p[(d*(d-1)+1):d^2])
# V.eps.inv.2= diag(p[(d^2+1):d*(d+1)])
# M=matrix(0,nrow = 23,ncol = 23)
# for(i in 1:23)
#    M[i,] = c(-p[(23*(i-1)+1):(23*(i-1)+i)],1,-p[(23*(i-1)+i):(23*i-1)])
# 