# SVM
library(e1071)

set.seed(1)
x = matrix(rnorm(2*20), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))

# add one to last 10 row of x
x[y == 1,] = x[y == 1] + 1
plot(x, col = (3 - y))

dat = data.frame(x = x, y = as.factor(y))

## apply SVM
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale = F)
plot(svmfit, dat)

## what are our support vectors?
svmfit$index

summary(svmfit)

## change cost
svm.fit = svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svm.fit, dat)  ### lower cost -> wider margines

# Cross-validation using tune
set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmodel = tune.out$best.model
summary(bestmodel)

## using predict function
## creating test data
xtest = matrix(rnorm(40), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))

## prediction
ypred = predict(bestmodel, testdat)
table(predict = ypred, truth = testdat$y)

## Using Radial kernels and playing with gamma and cost
## kernel = "radial" -> Uses radial kernel, you need to set gamma
## kernel = "polynomial" uses polynomial kernels
## for this you need to use degree component to fix it

set.seed(1)
x = matrix(rnorm(400), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2
y = c(rep(1, 50), rep(2, 50))
dat = data.frame(x=x, y = as.factor(y))
plot(x, col = y)
train = sample(200, 100)
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)

## change cost
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 10000)
plot(svmfit, dat[train,])

## find best model
tune.out = tune(svm, y~., data = dat[train,], kernel = "radial",
                range = list(cost = c(0.1, 1, 10, 100, 1000),
                             gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
plot(tune.out$best.model, dat[train,])
test = -train
table(true = dat[test, "y"], pred = predict(tune.out$best.model, news = dat[test,]))
