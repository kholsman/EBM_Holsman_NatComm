# Drake example

# Alternatively, install the development version from GitHub.
#install.packages("devtools")
#library(devtools)
#install_github("ropensci/drake")
# install.packages("packrat")
# Packrat bundle projects for easy sharing: https://github.com/rstudio/packrat

library(drake)
library(dplyr)
library(ggplot2)
library("downloader")

drake_example("main",to = "R/Drake_examples")
setwd("R/Drake_examples/main")


create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram()
}

# Get the files with drake_example("main").
file.exists("raw_data.xlsx")
file.exists("report.Rmd")

# plan what you are going to do:
#To insert a knitr report in a drake pipeline, use the 
#knitr_in() function inside your drake plan, and use 
#loadd() and readd() to refer to targets in the 
#report itself. See an example here.

plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data     = raw_data %>%mutate(Species = forcats::fct_inorder(Species)),
  hist     = create_plot(data),
  fit      = lm(Sepal.Width ~ Petal.Width + Species, data),
  report   = rmarkdown::render(
      knitr_in("report.Rmd"),
      output_file = file_out("report.html"),
      quiet = TRUE)
)
plan

vis_drake_graph(plan) # Interactive graph: zoom, drag, etc.

# now run the plan:

make(plan)
readd(data) 
readd(hist)

create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram(binwidth = 0.25) +
    theme_gray(20)
}

vis_drake_graph(plan) # Interactive graph: zoom, drag, etc.

make(plan)

loadd(hist)
hist

outdated(plan)






