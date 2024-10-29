#' @export
aleCI_plot <- function(ab)
    {
    if (class(ab$x.values) == "character") {
        ggplot2::ggplot(ab[,2:3]) +
        ggplot2::geom_bar( ggplot2::aes(x=as.factor(x.values),y=ab$f.values), stat='identity',color="light blue", fill="light blue") +
        ggplot2::geom_crossbar( ggplot2::aes(x=as.factor(x.values), y=ab$mean, ymin=ab$mean-2*ab$sd, ymax=ab$mean+2*ab$sd), color="red", alpha=0.9, linewidth=1, width=.2) +
        ggplot2::geom_point(data = ab[,2:3], ggplot2::aes(x=as.factor(x.values), y = ab$f.values), color='black', alpha=0.9, size=4)
        }
    else {
        ggplot2::ggplot(ab[,2:3]) +
        ggplot2::geom_ribbon( ggplot2::aes(x=ab$x.values, ymin=ab$mean-2*ab$sd, ymax=ab$mean+2*ab$sd), fill = "light blue") +
        ggplot2::geom_line( ggplot2::aes(x=ab$x.values, y=ab$f.values), color="black")
        }
    }
