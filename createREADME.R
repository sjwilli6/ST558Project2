# author: Spencer Williams and Stephen Macropoulos
# date: 2023-07-09
# purpose: Render Project2 as a .md file called README.md for our repo.

rmarkdown::render(
  input="ST558Project2.Rmd",
  output_format = "github_document",
  output_file = "README.md",
  runtime = "static",
  clean = TRUE,
  params = NULL,
  knit_meta = NULL,
  envir = parent.frame(),
  run_pandoc = TRUE,
  quiet = FALSE,
  encoding = "UTF-8"
)