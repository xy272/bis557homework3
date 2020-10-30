library(palmerpenguins)
library(usethis)
library(missForest)
library(dplyr)

penguinsi <- penguins %>%
  as.data.frame()%>%
  missForest() %>%
  `$`(ximp)%>%
  as_tibble()

use_data(penguinsi)

load("data/penguinsi.rda")
