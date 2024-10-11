#' @export
aleCI_plot <- function(X, J, fJ, x)
    {
    if (class(X[,J]) == "factor") {
        barplot(fJ, names=x, xlab=paste("x_", J, " (", names(X)[J], ")", sep=""), ylab= paste("f_",J,"(x_",J,")", sep=""), las =3)
        }
    else {
        plot(x, fJ, type="l", xlab=paste("x_",J, " (", names(X)[J], ")", sep=""), ylab= paste("f_",J,"(x_",J,")", sep=""))
        }
    }
