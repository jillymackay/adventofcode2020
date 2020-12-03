# Trees

# wtf
library(tidyverse)

trees <- readr::read_csv("day03/input.txt", col_names = c("tree"))


trees %>% 
  substr(tree, 3, 4)


substr(trees$tree, 3, 3)
