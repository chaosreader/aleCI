#plot with AleCI
library("AleCI")
yhat.mid <- function(newdata) mean(newdata)
yhat.low <- function(newdata) quantile(newdata, .025)[[1]]
yhat.high <- function(newdata) quantile(newdata, .975)[[1]]

#The Boston housing data
data("BostonHousing", package = "mlbench")
BH <- BostonHousing
BH$rad <- as.factor(BH$rad)
BH <- BH[,c(colnames(BH)[length(colnames(BH))],colnames(BH)[2:length(colnames(BH))-1])]

#train test split
set.seed(1234)
sample <- sample(c(TRUE, FALSE), nrow(BH), replace=TRUE, prob=c(0.7,0.3))
BH_train  <- BH[sample, ]
BH_test   <- BH[!sample, ]

#set the prediction function for the new model
yhat_ale <- function(object, newdata, type = pred_type) {
  as.numeric(predict(object, newdata)$predictions)
}

set.seed(1234)
df_models <- bag_train("ranger(
  formula        = medv ~ .,
  data           = BH_train)",
                       "BH_train",
                       200)

#include the training data for construction of the confidence intervals
df_data <- list()
df_data[[1]] <- BH_test
df_data[[2]] <- BH_train

#plot the ALE with AleCI
aleCI_plot(df_data,df_models, yhat_ale, yhat.mid, yhat.low, yhat.high,
           J = "age", K=40, NA.plot = TRUE)+
  ggplot2::xlab("Age") +
  ggplot2::ylab("Value") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

