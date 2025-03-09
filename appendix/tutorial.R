library("devtools")
install_github("chaosreader/aleCI")
library(AleCI)

# https://github.com/datasciencedojo/datasets/blob/master/titanic.csv

#download and clean the Titanic dataset

df <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/refs/heads/master/titanic.csv")
df <- df[ , c(colnames(df)[2],colnames(df)[1],colnames(df)[3:length(colnames(df))])]
df[is.na(df)] <- 0
df[df==""] <- "ZZ"
df$Cabin <- sapply(df$Cabin, function(x) substring(x, 1, 1))

df$Sex <- as.factor(df$Sex)
df$PassengerId <- as.factor(df$PassengerId)
df$Pclass <- as.factor(df$Pclass)
df$Name <- as.factor(df$Name)
df$SibSp <- as.factor(df$SibSp)
df$Parch <- as.factor(df$Parch)
df$Ticket <- as.factor(df$Ticket)
df$Cabin <- as.factor(df$Cabin)
df$Embarked <- as.factor(df$Embarked)

set.seed(1234)
# break Titanic data into train and test
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
df_train  <- df[sample, ]
df_test   <- df[!sample, ]

# create a biased training set
df_gender_bias <- df_train
df_gender_bias$Survived[df_gender_bias$Sex == 'male'] <- 0

library("ranger")

set.seed(1234)

#rf <- ranger(
#  formula        = Survived ~ .,
#  data           = df_gender_bias)
df_models <- bag_train("rf <- ranger(
  formula        = Survived ~ .,
  data           = df_gender_bias)",
          "df_gender_bias",
          1)

library("ALEPlot")
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
#ALEPlot(df_test[,2:12], df_models[[1]], yhat, J="Sex", K=10)
#source("./titanicALEPlot.R")
#titanicALEPlot(df_test[,2:12], df_models[[1]], yhat, J="Sex", K=10)

biased_model <- df_models[[1]]

set.seed(1234)
df_models <- bag_train("ranger(
    formula = Survived ~ .,
    data = df_train)",
          "df_train",
          200)

df_models[[1]] <- biased_model

#add the training data to build confidence intervals
df_data <- list()
df_data[[1]] <- df_test
df_data[[2]] <- df_train

#plot with AleCI
library("AleCI")
yhat.mid <- function(newdata) mean(newdata)
yhat.low <- function(newdata) quantile(newdata, .025)[[1]]
yhat.high <- function(newdata) quantile(newdata, .975)[[1]]

aleCI_plot(df_data, df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "Sex",
           K=10, NA.plot = TRUE)+
  ggplot2::xlab("Gender") +
  ggplot2::ylab("Survival") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

#replace biased model with unbiased model
set.seed(1234)
rf <- ranger(
  formula        = Survived ~ .,
  data           = df_train)
df_models[[1]] <- rf

#plot with AleCI
aleCI_plot(df_data, df_models, yhat, yhat.mid, yhat.low, yhat.high,
           J = "Sex", K=10, NA.plot = TRUE)+
  ggplot2::xlab("Gender") +
  ggplot2::ylab("Survival") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

aleCI_plot(df_data, df_models, yhat, yhat.mid, yhat.low, yhat.high,
           J = "Age", K=10, NA.plot = TRUE)+
  ggplot2::xlab("Age") +
  ggplot2::ylab("Survival") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

# https://mfasiolo.github.io/mgcViz/articles/miscellanea.html
library(mgcViz)
data(UKload)

set.seed(1234)

#fit the UK load example
fit <- qgam(NetDemand ~ Dow + s(Posan, k = 20) + s(wM) + s(wM_s95), data = UKload, qu = 0.5)

#plot the ALE with mgcViz
plot(ALE(fit, x = "wM_s95"))+
  ggplot2::xlab("Temperature") +
  ggplot2::ylab("Electric Load") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

#update the prediction function for the new model
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

#plot the ALE with ALEPlot
#source("./ukloadALEPlot.R")
#ukloadALEPlot(UKload, fit, yhat, J="wM_s95")

set.seed(1234)
df_models <- bag_train("qgam(NetDemand ~ Dow + s(Posan, k = 20) + s(wM) + s(wM_s95), data = UKload, qu = 0.5)","UKload", 200)

df <- as.data.frame(UKload)

#plot the ALE with AleCI
aleCI_plot(df,df_models, yhat, yhat.mid, yhat.low, yhat.high, J = "wM_s95",
           K=40, NA.plot = TRUE)+
  ggplot2::xlab("Temperature") +
  ggplot2::ylab("Electric Load") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

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

#train the model
set.seed(1234)
rf <- ranger(
  formula        = medv ~ .,
  data           = BH_train)

#set the prediction function for the new model
# yhat is the syntax for the origional ALEPlot, yhat_ale is not compatible with both
# ale and ALEPLot
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)

yhat_ale <- function(object, newdata, type = pred_type) {
  as.numeric(predict(object, newdata)$predictions)
}

#Plot ALE with ALEPLot
#source("./bostonALEPlot.R")
#bostonALEPlot(BH_test, rf , yhat, J="age", K=40)

#Plot ALE with ale with option to match ALEPlot values
library("ale")
ale_BH <- ale(data = BH_test, model = rf, pred_fun = yhat_ale, x_intervals = 40, relative_y = "zero" )
ale_BH$plots$age+
  ggplot2::xlab("Age") +
  ggplot2::ylab("Value") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

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

library("dplyr")
#load the diamonds data
diamonds <- ggplot2::diamonds |>
  filter(!(x == 0 | y == 0 | z == 0)) |>
  # https://lorentzen.ch/index.php/2021/04/16/a-curious-fact-on-the-diamonds-dataset/
  distinct(
    price, carat, cut, color, clarity,
    .keep_all = TRUE
  ) |>
  rename(
    x_length = x,
    y_width = y,
    z_depth = z,
    depth_pct = depth
  )

set.seed(1234)
#create gam model
gam_diamonds <- mgcv::gam(
  price ~ s(carat) + s(depth_pct) + s(table) + s(x_length) + s(y_width) + s(z_depth) +
    cut + color + clarity,
  data = diamonds
)

# Bootstraping is rather slow, so create a smaller subset of new data for demonstration
set.seed(1234)
new_rows <- sample(nrow(diamonds), 200, replace = FALSE)
diamonds_small_test <- diamonds[new_rows, ]

#remove outlier
diamonds2 <- diamonds_small_test[c(1:182,184:200), ]
max(diamonds2$carat)

#calulate ALE and CI with ale package
ale_gam_diamonds_boot <- ale(
  diamonds2, gam_diamonds,
  # Normally boot_it should be set to 100, but just 10 here for a faster demonstration
  boot_it = 10,
  parallel = 2  # CRAN limit (delete this line on your own computer)
)

# plot diamonds example with ale
ale_gam_diamonds_boot$plots$carat+
  ggplot2::xlab("Weight") +
  ggplot2::ylab("Value") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

#ale_gam_diamonds_boot$data$carat

# set the predition function to match the model
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

df <- as.data.frame(diamonds2)
df$cut <- factor(df$cut, ordered=FALSE)
df$color <- factor(df$color, ordered=FALSE)
df$clarity <- factor(df$clarity, ordered=FALSE)

#plot the ALE with ALEPlot
#source("./diamondsALEPlot.R")
#diamondsALEPlot(df[,c(1:6, 8:10)], gam_diamonds, yhat, J="carat", K=40)

#plot the ALE with mgcViz
plot(ALE(gam_diamonds, x = "carat", newdata=df[,c(1:6, 8:10)]))+
  ggplot2::xlab("Weight") +
  ggplot2::ylab("Value") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))

#train models for AleCI
set.seed(1234)
df_models <- bag_train("mgcv::gam(
  price ~ s(carat) + s(depth_pct) + s(table) + s(x_length) + s(y_width) + s(z_depth) +
    cut + color + clarity,
  data = diamonds)","diamonds",200)

aleCI_plot(df[,c(1:6, 8:10)],df_models, yhat, yhat.mid, yhat.low, yhat.high,
           J = "carat", K=40, NA.plot = TRUE)+
  ggplot2::xlab("Weight") +
  ggplot2::ylab("Value") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=20)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size=20))


