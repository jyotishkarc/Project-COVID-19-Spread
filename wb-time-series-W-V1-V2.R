library(dplyr)
library(expm)

# path.preamble <- paste0(getwd(),"/preamble.R")
# source(path.preamble)

#### First

E2 <- eigen(RVY2.inv)   # C2 = RVY2.inv
E2.val <- E2$values
E2.vec <- E2$vectors %>% t()

temp2 <- sqrtm(diag(E2.val)) %*% E2.vec
V2 <- diag(diag(temp2^2))

M <- solve(sqrtm(V2)) %*% temp2

target <- which(diag(M) < 0)
M[target, ] <- -M[target, ]

inv.M <- solve(M)

V1 <- t(inv.M) %*% RVY1.inv %*% inv.M


#### Second

E1 <- eigen(RVY1.inv)   # C2 = RVY2.inv
E1.val <- E1$values
E1.vec <- E1$vectors %>% t()

temp2 <- sqrtm(diag(E1.val)) %*% E1.vec
V1 <- diag(diag(temp2^2))

M <- solve(sqrtm(V1)) %*% temp2

target <- which(diag(M) < 0)
M[target, ] <- -M[target, ]

inv.M <- solve(M)

V2 <- t(inv.M) %*% RVY2.inv %*% inv.M
   





