#Build pdf from .Rmd files

library(renderthis)
library(pdftools)

to_pdf("Lecture1.html", complex_slides = TRUE, partial_slides = TRUE)
to_pdf("Tutorial1_presentation.html", complex_slides = TRUE, partial_slides = TRUE)
to_pdf("Tutorial2_presentation.html", complex_slides = TRUE, partial_slides = TRUE)
to_pdf("Tutorial3_presentation.html", complex_slides = TRUE, partial_slides = TRUE)
to_pdf("Intro_to_Rmarkdown.html", complex_slides = TRUE, partial_slides = TRUE)


