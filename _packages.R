# Install packages

my_packages <- c("tidyverse", "ggplot2", "cowplot", "colorblindr", "remotes", "xaringan", "xaringanExtra")
remotes::install_github("clauswilke/colorblindr")
remotes::install_github("jhelvy/xaringanBuilder", force = TRUE)
remotes::install_github('rstudio/chromote')
remotes::install_github('renderthis')
install.packages('pdftools')

forceinstall.packages(my_packages)

install.packages("colorspace")
install.packages("ggforce")
install.packages("kableExtra")
install.packages("ggpubr")

# install.packages("remotes")



