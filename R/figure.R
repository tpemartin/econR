#' Initiate Figure instance
#'
#' @return
#' @export
#'
#' @examples None
Figure <- function(){
  list(
    new=function(graphname){
      sym_graphname <- rlang::ensym(graphname)
      # browser()
      rlang::expr(
        generate_graph(!!sym_graphname)
      ) -> expr_genGraph
      rlang::eval_bare(expr_genGraph)
    }
  )
}

## helpers
generate_graph <- function(graphname){
  sym_graphname <- rlang::ensym(graphname)
  rlang::exprs(
    knitr::include_graphics("https://new.ntpu.edu.tw/assets/logo/ntpu.png")
  ) -> exprs_graph
  codeStart <- paste0("```{r ",
                      as.character(sym_graphname),", eval=T, echo=F, fig.cap='title', out.width='100%'}")
  c(
    codeStart,
    as.character(exprs_graph),
    "```",
    "",
    glue::glue("Figure \\@ref(fig:{as.character(sym_graphname)})")) -> str_graph
  clipr::write_clip(str_graph)
}

