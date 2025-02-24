aleCI_make_plot <- function(ad) {
    ab <- ad[[2]]
    ac <- ad[[1]]
  if (class(ab$x.values) == "character") {
    ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x=as.factor(ac$x.values),y=ac$f.values), stat='identity',color='black', fill='gray') +
      ggplot2::geom_errorbar(ggplot2::aes(x=as.factor(ac$x.values), ymin=ac$f.values, ymax=ac$f.values), color="black", alpha=0.8, linewidth=2, width=.9, lty = 1) +
      ggplot2::geom_errorbar(ggplot2::aes(x=as.factor(ab$x.values), ymin=ab$low, ymax=ab$high), color="dark green", alpha=1, linewidth=1, width=.75) +
      ggplot2::geom_errorbar(ggplot2::aes(x=as.factor(ab$x.values), ymin=ab$mean, ymax=ab$mean), color="dark green", alpha=.5, linewidth=1, width=.5, lty = 2)
  }
  else {
    ggplot2::ggplot() +
      ggplot2::geom_ribbon(ggplot2::aes(x=ab$x.values, ymin=ab$low, ymax=ab$high), fill = "forest green", alpha = .5) +
      ggplot2::geom_line(ggplot2::aes(x=ac$x.values, y=ac$f.values), color="black", linewidth=2) +
      ggplot2::geom_line(ggplot2::aes(x=ab$x.values, y=ab$low), color="dark green") +
      ggplot2::geom_line(ggplot2::aes(x=ab$x.values, y=ab$high), color="dark green")
  }
}
