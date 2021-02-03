produce_x <- function(L) 3*L
produce_y <- function(L) 4*L

# L_0 to L_0+1
L_0 <- 0
add1L_productivity <-
  {
    x_0 <- produce_x(L_0)

    L_next <- L_0 + 1
    x_next <- produce_x(L_next)

    add1L_productivity <- (x_next - x_0)/(L_next - L_0)
    add1L_productivity
  }

compute_add1L_productivity <- function(produce_x, L_0)
  {
    x_0 <- produce_x(L_0)

    L_next <- L_0 + 1
    x_next <- produce_x(L_next)

    add1L_productivity <- (x_next - x_0)/(L_next - L_0)
    add1L_productivity
  }

add1L_productivity_x <-
  compute_add1L_productivity(produce_x, 1:20)
add1L_productivity_y <-
  compute_add1L_productivity(produce_y, 1:20)

get_PPF <- function(endowment_L, produce_x, produce_y){
  PPFenv <- new.env()

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

PPF_A <- get_PPF(20, produce_x, produce_y)
PPF_A$plot_PPF()
PPF_A$update_endowmentL(30)
PPF_A$plot_PPF()
