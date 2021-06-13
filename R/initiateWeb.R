initiate_web <- function(){
  .GlobalEnv$drake <- rmd2drake:::Drake()
  .GlobalEnv$drake$source_plan()
  .GlobalEnv$drake$makePlan()
  .GlobalEnv$web <- econR::Web2()
}

