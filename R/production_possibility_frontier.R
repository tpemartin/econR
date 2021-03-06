#' Construct PPF
#'
#' @param endowment_L A number.
#' @param produce_x A production function of L input
#' @param produce_y A production function of L input
#'
#' @return An environment with above 3 input arguments, a plot_PPF function and an update_endowmentL function
#' @export
#'
#' @examples
#' produce_x <- function(L) 3*L
#' produce_y <- function(L) 4*L
#'
#' PPF_A <- get_PPF(20, produce_x, produce_y)
#' PPF_A$plot_PPF()
#' PPF_A$update_endowmentL(30)
#' PPF_A$plot_PPF()
get_PPF <- function(endowment_L, produce_x, produce_y){
  require(ggplot2)
  PPFenv <- new.env()

  PPFenv$endowment_L <- endowment_L
  PPFenv$produce_x <- produce_x
  PPFenv$produce_y <- produce_y
  PPFenv$`.yield_xyTradeoff` <- function(){
    Lx <- seq(0, PPFenv$endowment_L, length.out=102)
    Lx <- Lx[-c(1, length(Lx))]
    Ly <- PPFenv$endowment_L - Lx
    PPFenv$xy_tradeoff <-
      data.frame(
        x = PPFenv$produce_x(Lx),
        y = PPFenv$produce_y(Ly)
      )

  }
  PPFenv$`.yield_xyTradeoff`()
  PPFenv$plot_PPF <- function(...){
    require(ggplot2)
    ggplot()+
      geom_line(
        data=PPFenv$xy_tradeoff,
        mapping=aes(x=x,y=y)
      )
  }
  PPFenv$update_endowmentL <- function(endowment_L){
    PPFenv$endowment_L <- endowment_L
    xyOld <- PPFenv$xy_tradeoff
    PPFenv$`.yield_xyTradeoff`()
    PPFenv$plot_PPF <- function(...){
      require(ggplot2)
      ggplot()+
        geom_line(
          data=PPFenv$xy_tradeoff,
          mapping=aes(x=x,y=y), color="red"
        )+
        geom_line(
          data=xyOld,
          mapping=aes(x=x,y=y)
        )
    }
  }
  PPFenv
}
