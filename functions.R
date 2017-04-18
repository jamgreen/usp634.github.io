knit_rmds  <- function(dir) {
  require(rmarkdown)
  cwd <- getwd()
  nwd  <- file.path(cwd, dir)
  #setwd(nwd)
  
  for (rmd_file in list.files(path = nwd, pattern = "*.Rmd", full.names = TRUE)) {
    render(rmd_file)
  }
  
  #setwd(cwd)
}