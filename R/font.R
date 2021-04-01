#' Setup Chinese font for ggplot2
#'
#' @param need2Knit logical, default=F. TRUE means need to have all chunks default with fig.showtext=TRUE, fig.retina=1 for knitting Rmd purpose. If only need ggplot output to be exported as a graph file, default F is okay.
#'
#' @return
#' @export
#'
#' @examples None
setup_chinese <- function(need2Knit=F){
  if(need2Knit){
    if(!require(knitr)) install.packages("knitr")
    knitr::opts_chunk$set(fig.showtext=TRUE, fig.retina = 1)
  }

  if(!require(showtext)) install.packages("showtext")
  if(!require(ggplot2)) install.packages("ggplot2")
  sysfonts::font_add_google("Noto Sans TC", "Noto Sans TC")
  # font_add(
  #   "Noto Sans TC",
  #   regular="NotoSansTC-Regular.otf",
  #   bold="NotoSansTC-Bold.otf")
  showtext_auto()
  theme_set(
    theme(
      text=
        element_text(
          family = "Noto Sans TC")
    )
  )
}
