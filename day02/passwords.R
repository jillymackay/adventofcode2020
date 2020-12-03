# Passwords
# Day 2


library(tidyverse)


pass <- read.table("~/data/adventofcode2020/day02/input.txt", 
                    quote="\"", comment.char="") %>% 
  rename(rnum = V1,
         rlett = V2,
         pword = V3)


sample <- tibble(rnum = c("1-3", "1-3", "2-9"),
                 rlett = c("a:", "b:", "c:"),
                 pword = c("abcde", "cdefg", "ccccccccc"))

sample %>% 
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-") %>%
  mutate(rlett = str_extract(rlett, "[abc]")) %>% 
  mutate(v1 = str_count(pword, rlett),
         valid = case_when(v1 >= rmin & v1 <= rmax ~ "Valid",
                           TRUE ~ "Not Valid")) %>% 
  filter(valid == "Valid") %>% 
  tally()


# I do not understand why this doesn't work
pass %>%   
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-") %>%
  mutate(rlett = str_extract(rlett, "[a-z]")) %>% 
  mutate(lettinst = str_count(pword, rlett),
         valid = case_when(lettinst >= rmin & lettinst <= rmax ~ "Valid",
                           TRUE ~ "Not Valid")) %>% 
  filter(valid == "Valid") %>% 
  tally()



# When in doubt - look at what Ian did ...


ipass <-   read_csv("input.txt", col_names = "policy") %>% 
  extract(policy,
          c("min", "max", "letter", "password"),
                   "(\\d+)-(\\d+) (.): *(.*)",
                   convert = TRUE
)
