#' Generate drake object ui browsable with dependencies attachment
#'
#' @param main_dependencies a taglist of html dependencies
#'
#' @return a function that attaches main_dependencies to an html objects in drake for browsing
#' @export
#'
#' @examples none
generate_drakebrowsable <- function(main_dependencies){
  sym_main_dependencies <- rlang::ensym(main_dependencies)
  # #browser()
  # paste0("drake$loadTarget$", as.character(sym_main_dependencies),"()") -> chr_dep_expr

  function(ui){
    sym_ui <- rlang::ensym(ui)
    paste0("drake$loadTarget$", as.character(sym_ui),"()") -> chr_expr
    eval(parse(text=chr_expr), envir = .GlobalEnv)

    # browser()
    # # drake load main_dependencies if not exists
    flag_exist_maindep <- exists(
      as.character(sym_main_dependencies), envir = .GlobalEnv
    )
    if(!flag_exist_maindep) load_mainDependencies(sym_main_dependencies)
    # if(!flag_exist_maindep) eval(parse(text=chr_dep_expr), envir = .GlobalEnv)

    rlang::expr(
      htmltools::browsable(
        htmltools::tagList(
          !!sym_ui,
          !!sym_main_dependencies
        )
      )
    ) -> expr_browsable
    rlang::eval_bare(
      expr_browsable, env=.GlobalEnv
    )
  }
}
#' Generate browsable with dependencies attachment
#'
#' @param main_dependencies a taglist of html dependencies
#'
#' @return a function that attaches main_dependencies to an html objects for browsing
#' @export
#'
#' @examples none
generate_browsable <- function(main_dependencies){
  function(ui){
    htmltools::browsable(
      htmltools::tagList(
        ui,
        main_dependencies
      )
    )
  }
}
web_browsable <-
  function(experimental_element, rows="s12"){
    require(htmltools)
    require(htmlwidgets)


    # rows="s12"
    # experimental_element = expel
    ## ----bodyTags-----------------------------------------------------------------------------
    bodyTags <- {
      htmltools::tagList(
        htmltools::tags$div(class="container",
                 htmltools::tags$div(class="row",
                          htmltools::tags$div(class=glue::glue("col {rows}"),
                                   experimental_element)
                 ))
      )
    }
    ## ----headTags-----------------------------------------------------------------------------
    headTags <- {
      htmltools::tagList(
        htmltools::tags$link(
          href="https://fonts.googleapis.com/icon?family=Material+Icons",
          rel="stylesheet"
        )
      )
    }


    ## ----html_placeholder---------------------------------------------------------------------
    html_placeholder <- htmltools::tags$html(
      htmltools::tags$head(
        do.call(htmltools::tagList, headTags),
        htmltools::tags$meta(
          name="viewport",
          content="width=device-width, initial-scale=1.0"
        )
      ),
      htmltools::tags$body(
        do.call(htmltools::tagList, bodyTags)
      )
    )


    ## ----myDependency-------------------------------------------------------------------------
    .root2 <- rprojroot::is_rstudio_project$make_fix_file()
    myDependency <- htmltools::htmlDependency(
      name="myown",
      version="1.0",
      src=c(filepath=.root2()),
      stylesheet = "css/mystyle.css",
      script = "js/mydesign.js",
      head = '<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Noto+Sans+TC">
  '
    )


    ## ----local_jquery-------------------------------------------------------------------------
    local_jquery <-
      htmltools::htmlDependency(
        name="jquery",
        version="3.5.1",
        src=c(href = "lib/jquery-3.5.1"),
        # to use the same library here must use href, not filepath; otherwise, the current jquery system will be removed.
        script = c("jquery.min.js")
      )


    ## ----html_complete------------------------------------------------------------------------
    html_complete <-
      htmltools::tagList(
        html_placeholder,
        .GlobalEnv$web$htmlDependencies$materialise(),
        local_jquery,
        myDependency
      )


    ## ----save_html----------------------------------------------------------------------------
    destfile = file.path(dirname(.GlobalEnv$web$output_filepath()), "_browsable.html")

    htmltools::save_html(
      html_complete,
      file = destfile,
      libdir="lib"
    )


    ## -----------------------------------------------------------------------------------------
    # web_temp$browse()
    rstudioapi::viewer(
      destfile
    )
    # browser()
    # htmltools::browsable(html_complete)
    invisible(html_complete)
  }

#' Browsable for Materialise CSS framework
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples None
browsable_materialise <- function(...){
  dep <- html_dependency()
  htmltools::tagList(
    dep$materialise(),
    dep$jquery(),
    ...
  ) -> html2browse
  htmltools::browsable(html2browse)
}
load_mainDependencies <- function(sym_mainDep){
  paste0("eval(drake$loadTarget$", as.character(sym_mainDep), "(),
    envir = .GlobalEnv)") -> chr_expr
  eval(parse(text=chr_expr), envir = .GlobalEnv)
}
