# Why is this different?

library(tidyverse)


passfun <- function(x) {
  x %>% 
    mutate(lettinst = str_count(pword, rlett),
         valid = case_when(lettinst >= rmin & lettinst <= rmax ~ "Valid",
                           TRUE ~ "Not Valid")) %>% 
    filter(valid == "Valid") %>% 
    tally()
  }


working_pass <- read_csv("~/data/adventofcode2020/day02/input.txt", col_names = "pass") %>% 
  extract(pass,
          c("rmin", "rmax", "rlett", "pword"),
          "(\\d+)-(\\d+) (.): *(.*)",
          convert = TRUE
  )

fail_pass <- read.table("~/data/adventofcode2020/day02/input.txt") %>% 
  rename(rnum = V1,
         rlett = V2,
         pword = V3) %>% 
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-") %>% 
  mutate(rlett = str_extract(rlett, "[a-z]"))


passfun(working_pass)
passfun(fail_pass)

