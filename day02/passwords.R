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


ipass <-   read_csv("~/data/adventofcode2020/day02/input.txt", col_names = "policy") %>% 
  extract(policy,
          c("min", "max", "letter", "password"),
                   "(\\d+)-(\\d+) (.): *(.*)",
                   convert = TRUE
)
# this processes the data as far as I did to line 33

ipass %>% 
  mutate(count = map2_int(password, letter, str_count)) %>% 
  filter(count >= min & count <= max)

# s my str_count isn't working right ....

# why .......



pass %>%   
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-") %>%
  mutate(rlett = str_extract(rlett, "[a-z]")) %>% 
  mutate(lettinst = map2_int(pword, rlett, str_count)) %>% 
  filter(lettinst >= rmin & lettinst <= rmax) %>% 
  tally()

# Okay so no - its probably my data processing???




ipass %>%
  rename(rmin = min,
         rmax = max,
         rlett = letter, 
         pword = password)  %>% 
  mutate(lettinst = str_count(pword, rlett),
       valid = case_when(lettinst >= rmin & lettinst <= rmax ~ "Valid",
                         TRUE ~ "Not Valid")) %>% 
  filter(valid == "Valid") %>% 
  tally()


# So my process works, its just my reading of the data????


pass <- read_csv("~/data/adventofcode2020/day02/input.txt", col_names = "pass")
p1 <- read.table("~/data/adventofcode2020/day02/input.txt") %>% 
  rename(rnum = V1,
         rlett = V2,
         pword = V3) %>% 
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-") %>% 
  mutate(rlett = str_extract(rlett, "[a-z]"))






working_pass <- read_csv("~/data/adventofcode2020/day02/input.txt", col_names = "pass") %>% 
  extract(policy,
          c("rmin", "rmax", "rlett", "rpass"),
          "(\\d+)-(\\d+) (.): *(.*)",
          convert = TRUE
  )

fail_pass <- read.table("~/data/adventofcode2020/day02/input.txt") %>% 
  rename(rnum = V1,
         rlett = V2,
         pword = V3) %>% 
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-") %>% 
  mutate(rlett = str_extract(rlett, "[a-z]"))

















# Stage 2


sample %>% 
  separate(col = rnum, into = c("rmin", "rmax"), sep = "-", convert = TRUE) %>%
  mutate(rlett = str_extract(rlett, "[abc]")) %>%
  mutate(v1 = (str_locate_all(pword, rlett)),
         v2 = list(rmin) %in% v1)
  
  
  mutate(v1 = str_count(pword, rlett),
         valid = case_when(v1 >= rmin & v1 <= rmax ~ "Valid",
                           TRUE ~ "Not Valid")) %>% 
  filter(valid == "Valid") %>% 
  tally()