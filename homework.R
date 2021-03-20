# ****************************************
# This R script is for checking if students handed in their assignmnet
#
# Last updated: Sujin Lee, 2021 March
#
# How to use:
#   - Download all the assignments and save in a new folder named "hw_due(date)" 
#   - Copy and Paste 3/17 and modify the followings: 
#        1) date march17, "2021-03-17", "0317"
#        2) groups that need to hand in the homework
#        3) students who are exempt from handing in hw
# ****************************************

# install.packages("pacman")
library(pacman)
p_load("tidylog", "tidyverse", "readxl", "magrittr", "stringr", "data.table", "lubridate", "writexl")


#### List of all students ####

list <- read_excel("G:/공유 드라이브/class name/participants_list.xlsx")

list <- list[-1]
colnames(list) <- c("major", "id", "name", "type", "email", "hp")

students <- list %>%
  filter(type == "학생") %>%
  dplyr::select(major, id, name)
# filter only students

students %<>% 
  mutate(
    grp = case_when(
      id >= "2012-xxxxx" & id <= "2015-xxxxx" ~ 1,
      id >= "2015-xxxxx" & id <= "2016-xxxxx" ~ 2,
      id >= "2016-xxxxx" & id <= "2017-xxxxx" ~ 3,
      id >= "2017-xxxxx" & id <= "2018-xxxxx" ~ 4,
      id >= "2018-xxxxx" & id <= "2021-xxxxx" ~ 5,
      TRUE ~ NA_real_)
  )
# group students


##### due 3/17 ####

hw <- list.files("G:/공유 드라이브/class name/hw_due0317/")

hw2 <- str_split_fixed(hw, "_", n = 3)[, 1:2]

colnames(hw2) <- c("name", "id")

hw2 %<>% as.data.frame() %>%
  mutate(
    due_0317 = 0, 
    score_0317 = NA
    )

students %<>% left_join(., hw2, by = c("id", "name"))

students %<>% 
  mutate(
    due_0317 = case_when(
      grp %in% c(2:3) & is.na(due_0317) ~ "1", 
      # 3/17 제출 대상 조인데 미제출이면 1, 제출 대상 아니면 NA
      TRUE ~ as.character(due_0317)
      ),
    due_0317 = case_when(
      name %in% c("name of students") ~ "면제",
      # 면제자
      TRUE ~ as.character(due_0317)
    )
  )

write_xlsx(students, "G:/공유 드라이브/class name/hw_due0317.xlsx")
# insert the scores of each student in hw_due0317.xlsx and save it to  "hw_due0317_scored.xlsx"


