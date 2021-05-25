#' Initiate table instance
#'
#' @return
#' @export
#'
#' @examples None
Table <- function(){
  list(
    new = function(tablename, filename=NA){
      sym_tablename <- rlang::ensym(tablename)
      # browser()
      rlang::expr(
        generate_newTable(!!sym_tablename, filename)
      ) -> expr_genTable
      rlang::eval_bare(expr_genTable)
    }
  )
}


# Helpers -----------------------------------------------------------------


generate_newTable <- function(tablename, filename=NA){
  quo_tablename <- rlang::ensym(tablename)

  if(is.na(filename)){
    filename=paste0(quo_tablename,".Rdata")
  } else {
    filename=paste0(tablename,".Rdata")
  }

  table_dir <- {
    rstudioapi::getSourceEditorContext() -> activeX
    file.path(dirname(activeX$path),"table")
  }
  if(!dir.exists(table_dir)) dir.create(table_dir)

  filepath =
    file.path(table_dir, filename)

  quo_filepath = rlang::quo(filepath)

rlang::expr(
    save(!!quo_tablename, file=!!filepath)
  ) -> expr_tableSave
  rlang::expr(
    !!quo_tablename <- clipr::read_clip_tbl()
  ) -> expr_readClip


  rlang::eval_bare(expr_readClip, env = .GlobalEnv)
  rlang::eval_bare(expr_tableSave)

  rlang::expr(
    load(!!filepath)
  ) -> expr_loadTable
  rlang::exprs(
    load(!!filepath),
    kbl(!!quo_tablename,caption="title") %>%
      kable_classic(full_width = F) %>%
      footnote(
        general_title = "附註: ",
        general = "footnote"
      )
  ) -> exprs_table
  codeStart <- paste0("```{r ",
                      as.character(quo_tablename),", eval=T, echo=F}")
  c(
    codeStart,
  as.character(exprs_table),
  "```",
  "",
  glue::glue("Table \\@ref(tab:{as.character(quo_tablename)})")) -> str_table
  clipr::write_clip(str_table)
  # clipr::write_clip(
  #   c(
  #     rlang::expr_deparse(expr_loadTable),
  #     rlang::expr_deparse(quo_tablename)
  #   )
  # )

  # browser()
}
