library(glmnet)
library(dplyr)
library(tidyr)
y=districts.cleaned[,1]
x=districts.cleaned[,-1]
grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge_mod))
plot(ridge_mod)
ridge_mod$lambda[50] #Display 50th lambda value
coef(ridge_mod)[,50] # Display coefficients associated with 50th lambda value
sqrt(sum(coef(ridge_mod)[-1,50]^2)) # Calculate l2 norm

ridge_mod$lambda[78] #Display 60th lambda value
coef(ridge_mod)[,78] # Display coefficients associated with 60th lambda value
sqrt(sum(coef(ridge_mod)[-1,78]^2)) # Calculate l2 norm

set.seed(1)

train = districts.cleaned %>%sample_frac(0.5)

test = districts.cleaned %>%setdiff(train)

x_train = model.matrix(Alipurduar~., train)[,-1]
x_test = model.matrix(Alipurduar~., test)[,-1]

y_train = train[,1]
y_test = test[,1]

set.seed(1)
cv.out = cv.glmnet(x, y, alpha = 0) # Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lamda that minimizes training MSE
bestlam
plot(cv.out)

out = glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
