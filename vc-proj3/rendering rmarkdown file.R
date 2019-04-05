library(knitr)
library(rmarkdown)

# rendering rmarkdown file to rtf
# "05 Ekplorasi 1 Proj3"
# "06 Ekplorasi 2 Proj3"
# "08 Petisi Teluk Benoa"
# "12 Media Data"

rmarkdown::render("05 Ekplorasi 1 Proj3.Rmd", "html_notebook")
rmarkdown::render("06 Ekplorasi 2 Proj3.Rmd", "html_notebook")


rmarkdown::render("05 Ekplorasi 1 Proj3.Rmd", "pdf_document")


