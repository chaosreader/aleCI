bag_train <- function(train.func, var.data, nmodels=20, my.env=environment()){
    model <- eval(train.func)
    df_models = list(type=class(model))
    df_models[[1]] <- model
    df_long = var.data[sample(nrow(var.data), (nrow(var.data)*nmodels), replace=TRUE), ]
    rownames(df_long) <- NULL
    n = 1
    for(i in seq(from=1, to=nrow(df_long)-1, by=(nrow(df_long)/nmodels))){
      n = n + 1
      bag.data <- df_long[i:(i+(nrow(df_long)/nmodels-1)),]
      bag.func <- sub("var.data", "bag.data", train.func)
      model <- eval(bag.func)
      df_models[[n]] <- model
        }
    return(df_models)
    }
