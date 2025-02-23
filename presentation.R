library("dplyr")
library("randomForest")
library("ICEbox")
library("reprtree")
library("mlbench")
library("caret")
library("pdp")
library("ranger")
library("MASS")
library("ALEPlot")
library("ggplot2")
library("AleCI")

set.seed(1234)

newboston <- Boston
newboston$rad <- as.factor(newboston$rad)
newboston$chas <- as.factor(newboston$chas)

sample <- sample(c(TRUE, FALSE), nrow(newboston), replace=TRUE, prob=c(0.7,0.3))
b_train <- newboston[sample, ]
row.names(b_train) <- NULL
b_test <- newboston[!sample, ]
row.names(b_test) <- NULL

rf_boston <- randomForest(
  formula = medv ~.,
  data = b_train)

#https://stackoverflow.com/questions/16548882/how-to-know-if-a-regression-model-generated-by-random-forests-is-good-mse-and
# model stats
rf_boston
Est <- as.numeric(predict(rf_boston, b_test))
class(Est)
plot(b_test$medv, Est, main="Prediction vs Actual", xlab = "median value", ylab = "predicted median value")

reprtree:::plot.getTree(rf_boston, depth=4)
reprtree:::plot.getTree(rf_boston)

varImpPlot(rf_boston, sort=TRUE, main="Variable Importance Plot")

yhat <- function(object, newdata) as.numeric(predict(object, newdata))
rm.ice = ice(object = rf_boston, X = b_train, y = b_train$medv, predictor = "rm", frac_to_build = 1)
# rm ice plot
plot(rm.ice, x_quantile = FALSE, plot_pdp = FALSE, frac_to_plot = 1, centered = FALSE,
     main="ICE plot for rooms vs median value", xlab = "rooms", ylab = "median value")

# age ice no pdp
age.ice = ice(object = rf_boston, X = b_train, y = b_train$medv, predictor = "age", frac_to_build = 1)
plot(age.ice, x_quantile = FALSE, plot_pdp = FALSE, frac_to_plot = 1, centered = FALSE,
     main="ICE plot for age vs median value", xlab = "age", ylab = "median value")

#just pdp
mypartial <- partial(rf_boston, pred.var = "age", plot = TRUE)
mypartial
# age ice with pdp
plot(age.ice, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, centered = FALSE,
     main="ICE with PDP plot for age vs median value", xlab = "age", ylab = "median value")

# rm ice with pdp
plot(rm.ice, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, centered = FALSE)

#ice room with pdp
plot(rm.ice, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, centered = FALSE,
     main="ICE plot for rooms vs median value", xlab = "rooms", ylab = "median value")

#aleCI
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
yhat.low <- function(newdata) quantile(newdata, .025)[[1]]
yhat.mid <- function(newdata) median(newdata)
yhat.high <- function(newdata) quantile(newdata, .975)[[1]]
model_list <- list()
model_list[[1]] <- rf_boston
model_list[[2]] <- rf_boston

numbers <- aleCI(b_train, model_list, yhat, yhat.mid, yhat.low, yhat.high, "rm", 10)
aleCI_make_plot(numbers) +
  xlab("rooms") +
  ylab("change to median value")

ALEPlot(b_train, rf_boston, yhat, "rm", 10)

myplot <- numbers[[1]]

ggplot(myplot[,2:3], aes(x.values, f.values)) +
  geom_point() +
  geom_segment(aes(x = x.values[1], y=f.values[1], xend = x.values[2], yend=f.values[2], colour = "segment"))+
  geom_segment(aes(x = x.values[2], y=f.values[2], xend = x.values[3], yend=f.values[3], colour = "segment")) +
  geom_segment(aes(x = x.values[3], y=f.values[3], xend = x.values[4], yend=f.values[4], colour = "segment"))+
  geom_segment(aes(x = x.values[4], y=f.values[4], xend = x.values[5], yend=f.values[5], colour = "segment"))+
  geom_segment(aes(x = x.values[5], y=f.values[5], xend = x.values[6], yend=f.values[6], colour = "segment"))+
  geom_segment(aes(x = x.values[6], y=f.values[6], xend = x.values[7], yend=f.values[7], colour = "segment"))+
  geom_segment(aes(x = x.values[7], y=f.values[7], xend = x.values[8], yend=f.values[8], colour = "segment"))+
  geom_segment(aes(x = x.values[8], y=f.values[8], xend = x.values[9], yend=f.values[9], colour = "segment"))+
  geom_segment(aes(x = x.values[9], y=f.values[9], xend = x.values[10], yend=f.values[10], colour = "segment"))+
  geom_segment(aes(x = x.values[10], y=f.values[10], xend = x.values[11], yend=f.values[11], colour = "segment"))+
  xlab("rooms") +
  ylab("change to median value")

mypartplot <- myplot
mypartplot$f.values <- myplot$f.values - myplot$f.values[c(1,1:10)]

ggplot(myplot[,2:3]) +
  geom_segment(aes(x = mypartplot$x.values[1], y=0, xend = mypartplot$x.values[2], yend=mypartplot$f.values[2], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[2], y=0, xend = mypartplot$x.values[3], yend=mypartplot$f.values[3], colour = "segment")) +
  geom_segment(aes(x = mypartplot$x.values[3], y=0, xend = mypartplot$x.values[4], yend=mypartplot$f.values[4], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[4], y=0, xend = mypartplot$x.values[5], yend=mypartplot$f.values[5], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[5], y=0, xend = mypartplot$x.values[6], yend=mypartplot$f.values[6], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[6], y=0, xend = mypartplot$x.values[7], yend=mypartplot$f.values[7], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[7], y=0, xend = mypartplot$x.values[8], yend=mypartplot$f.values[8], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[8], y=0, xend = mypartplot$x.values[9], yend=mypartplot$f.values[9], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[9], y=0, xend = mypartplot$x.values[10], yend=mypartplot$f.values[10], colour = "segment"))+
  geom_segment(aes(x = mypartplot$x.values[10], y=0, xend = mypartplot$x.values[11], yend=mypartplot$f.values[11], colour = "segment"))+
  xlab("rooms") +
  ylab("change to median value")

nmodels = 9
df_models = list(type="randomForest")
df_long = b_train[sample(nrow(b_train), (350*nmodels), replace=TRUE), ]
rownames(df) <- NULL

n = 0
for(i in seq(from=1, to=nrow(df_long)-1, by=(nrow(df_long)/nmodels))){
  n = n + 1
  rf <- randomForest(
    formula        = medv ~ .,
    data           = df_long[i:(i+(nrow(df_long)/nmodels-1)),])
  df_models[[n]] <- rf
}

df_models[[n+1]] <-rf_boston
df_models <- rev(df_models)

rf_dots <- list()
for (i in 1:10) {
  rf_dots[[i]] <- aleCI(b_train, list(df_models[[i]], df_models[[i]]), yhat, yhat.mid, yhat.low, yhat.high, J = "rm",
                        K=10)[[2]]
}

aa <- aleCI(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "rm",
          K=10)

ggplot(rf_dots[[1]][,2:3], aes(x.values, f.values)) +
  geom_point() +
  geom_line() +
  geom_point(aes(x.values, f.values), data=rf_dots[[2]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[3]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[4]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[5]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[6]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[7]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[8]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[9]][,2:3]) +
  geom_point(aes(x.values, f.values), data=rf_dots[[10]][,2:3])+
  xlab("rooms") +
  ylab("change to median value")

# Demo

set.seed(1234)

#create a hundred models
nmodels = 99
df_models = list(type="randomForest")
df_long = b_train[sample(nrow(b_train), (350*nmodels), replace=TRUE), ]
rownames(df) <- NULL

n = 0
for(i in seq(from=1, to=nrow(df_long)-1, by=(nrow(df_long)/nmodels))){
  n = n + 1
  rf <- randomForest(
    formula        = medv ~ .,
    data           = df_long[i:(i+(nrow(df_long)/nmodels-1)),])
  df_models[[n]] <- rf
}

df_models[[n+1]] <-rf_boston
df_models <- rev(df_models)

# set prediction function (inherited from apley and other R programmer)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

# set bounding functions
yhat.low <- function(newdata) quantile(newdata, .025)[[1]]
yhat.mid <- function(newdata) median(newdata)
yhat.high <- function(newdata) quantile(newdata, .975)[[1]]

#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "crim", K=10)
#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "zn", K=10)
aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "indus", K=10)+
  xlab("proportion of non-retail business acres per town.") +
  ylab("change to median value")
#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "chas", K=10)
aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "rm", K=10)+
  xlab("rooms") +
  ylab("change to median value")
aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "age", K=10)+
  xlab("age") +
  ylab("change to median value")
#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "dis", K=10)
aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "rad", K=10)+
  xlab("index of accessibility to radial highways.") +
  ylab("change to median value")
#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "tax", K=10)
#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "ptratio", K=10)
#aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "lstat", K=10)

# recover ALEPlot data
rad_data <- aleCI(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "rad", K=10)

rad_data[[1]]

ALEPlot(b_train, df_models[[1]], yhat, J="rad", K=10)


# switch quantiles for std deviation 99% CI
yhat.low <- function(newdata) mean(newdata) - 2.576*(sd(newdata))
yhat.mid <- function(newdata) mean(newdata)
yhat.high <- function(newdata) mean(newdata) + 2.576*(sd(newdata))
aleCI_plot(b_train, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "age", K=10)


# place CI around test data
aleCI_plot(list(b_test, b_train), df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "rm", K=10)+
  xlab("rooms") +
  ylab("change to median value")
max(b_train$rm)
min(b_train$rm)
max(b_test$rm)
min(b_test$rm)

# build CI with all data and graph test data
aleCI_plot(list(b_test, newboston), df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "rm", K=10)+
  xlab("rooms") +
  ylab("change to median value")

