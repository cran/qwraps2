## ----label=setup, include = FALSE----------------------------------------
library(knitr)
knitr::opts_chunk$set(collapse = TRUE)

## ------------------------------------------------------------------------
library(qwraps2)

## ------------------------------------------------------------------------
str(create_pkg)

## ------------------------------------------------------------------------
tmp_dir <- tempdir()
pkg_dir <- paste(tmp_dir, "eg.pkg", sep = "/")
pkg_dir
create_pkg(pkg_dir)

## ------------------------------------------------------------------------
tree <- function(pkg_dir) {
  files <- list.files(pkg_dir,
                      all.files = TRUE,
                      full.names = TRUE,
                      recursive = TRUE,
                      include.dirs = TRUE)
  files <- data.frame(filename = gsub(tmp_dir, "", files), file.info(files)) 
  print(data.tree::as.Node(files, pathName = "filename"), "isdir")
}
tree(pkg_dir)

## ------------------------------------------------------------------------
cat(readLines(paste0(pkg_dir, "/DESCRIPTION")), sep = "\n")

## ------------------------------------------------------------------------
# cat(readLines(paste0(pkg_dir, "/makefile")), sep = "\n")

## ------------------------------------------------------------------------
create_data_raw(pkg_dir)

## ------------------------------------------------------------------------
tree(pkg_dir)

## ------------------------------------------------------------------------
cat(readLines(paste0(pkg_dir, "/data-raw/makefile")), sep = "\n")

## ------------------------------------------------------------------------
create_vignette(name = "egVign.R", path = pkg_dir)
tree(pkg_dir)

## ------------------------------------------------------------------------
print(sessionInfo(), local = FALSE)

