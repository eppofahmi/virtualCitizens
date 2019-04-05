# Load packages
require(knitr)
require(markdown)

setwd("/Volumes/mydata/RStudio/virtualCitizens/vc-proj3/R")

# Create .md, .html, and .pdf files
knit("RangkumanV3.Rmd")
markdownToHTML('My_Analysis.md', 'My_Analysis.html', options=c("use_xhml"))
system("pandoc -s My_Analysis.html -o My_Analysis.pdf")