## convert markdown doc to read me file for github
rmd2md <- function(rmd_fl = "README_Holsman_EBMpaper",md_fl = "README"){
  library(rmarkdown)
  render(paste0(rmdfl,".Rmd"), md_document(variant = "markdown_github"),params=F)
  file.copy(from=paste0(rmdfl,".md"),to=paste0(".md"),overwrite=T)
}

