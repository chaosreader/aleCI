bag_train <- function(train.func, var.data, nmodels=20, my.env=environment()){
    model <- eval(parse(text=train.func))
    df_models = list(type=class(model))
    df_models[[1]] <- model
    df_long = eval(parse(text=var.data))[sample(nrow(eval(parse(text=var.data))), (nrow(eval(parse(text=var.data)))*(nmodels-1)), replace=TRUE), ]
    rownames(df_long) <- NULL
    n = 1
    for(i in seq(from=1, to=nrow(df_long)-1, by=(nrow(df_long)/nmodels))){
      print("entered loop")
      n = n + 1
      bag.data <- df_long[i:(i+(nrow(df_long)/nmodels-1)),]
      bag.func <- sub(var.data, "bag.data", train.func)
      model <- eval(parse(text=bag.func))
      df_models[[n]] <- model
        }
    return(df_models)
    }
