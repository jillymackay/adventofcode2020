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

#Learning from Ian that 'crossing' is a thing after trying for loops
input %>% 
  crossing(V2 = .$V1) %>% 
  crossing(V3 = .$V1) %>% 
  filter(V1 + V2  + V3 == 2020) %>% 
  summarise(product  = V1 * V2 * V3)


