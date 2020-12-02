# Sum 2020 problem
# Day 1

library(tidyverse)


input <- read.table("~/data/adventofcode2020/day01/input.txt", 
                    quote="\"", comment.char="") 


ninput <- input %>% 
  mutate(V1 = 2020-V1)

ninput %>%
  semi_join(input) 

1703*317



# Stage  2 - find three numbers  
