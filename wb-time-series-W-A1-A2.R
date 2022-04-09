library(dplyr)
library(expm)

# path.preamble <- paste0(getwd(),"/preamble.R")
# source(path.preamble)

E2 <- eigen(RVY2.inv)
E2.val <- E2$values
E2.vec <- E2$vectors %>% t()

temp2 <- sqrtm(diag(E2.val)) %*% E2.vec
V2 <- diag(diag(temp2^2))

M <- solve(sqrtm(V2)) %*% temp2

target <- which(diag(M) < 0)
M[target, ] <- -M[target, ]

inv.M <- solve(M)

V1 <- t(inv.M) %*% RVY1.inv %*% inv.M
   





