aleCI_make_plot <- function(ad)
{
    ab <- ad[[2]]
    ac <- ad[[1]]
  if (class(ab$x.values) == "character") {
    ggplot2::ggplot(ab[,2:3]) +
      ggplot2::geom_bar(ggplot2::aes(x=as.factor(x.values),y=ac$f.values), stat='identity',color='pale green', fill='pale green') +
      ggplot2::geom_errorbar(ggplot2::aes(x=as.factor(x.values), ymin=ab$low, ymax=ab$high), color="forest green", alpha=0.75, linewidth=1, width=.6) +
      ggplot2::geom_errorbar(ggplot2::aes(x=as.factor(x.values), ymin=ab$mean, ymax=ab$mean), color="forest green", alpha=0.9, linewidth=1, width=.6, lty = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(x=as.factor(x.values), ymin=ac$f.values, ymax=ac$f.values), color="black", alpha=0.9, linewidth=1, width=.9, lty = 1)
    #ggplot2::geom_crossbar( ggplot2::aes(x=as.factor(x.values), y=ab$mean, ymin=ab$mean-2*ab$sd, ymax=ab$mean+2*ab$sd), color="forest green", alpha=0.9, linewidth=1, width=.2) +
    #ggplot2::geom_pointrange( ggplot2::aes(x=as.factor(x.values), y=ab$mean, ymin=vab$mean-2*ab$sd, ymax=ab$mean+2*ab$sd), colour="orange", alpha=0.9, size=1.3)+
    #ggplot2::geom_point(data = ab[,2:3], ggplot2::aes(x=as.factor(x.values), y = ab$f.values), color='black', alpha=0.9, size=4)
  }
  else {
    ggplot2::ggplot(ab[,2:3]) +
      ggplot2::geom_ribbon(ggplot2::aes(x=ab$x.values, ymin=ab$low, ymax=ab$high), fill = "pale green") +
      ggplot2::geom_line(ggplot2::aes(x=ac$x.values, y=ac$f.values), color="black", linewidth=2) +
      ggplot2::geom_line(ggplot2::aes(x=ab$x.values, y=ab$low), color="forest green") +
      ggplot2::geom_line(ggplot2::aes(x=ab$x.values, y=ab$high), color="forest green")
  }
}

