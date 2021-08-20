##### 경제원론 출결 #####

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(tidylog)

# 학생 이름 목록
list <- read_excel("../경제원론1(2021년여름학기)_이수진.xlsx", skip = 1)
colnames(list) <- c("a", "b", "major", "id", "name")

list2 <- list %>% dplyr::select(id, name) %>% drop_na(id)


# 출결 기록 파일 불러오기
files_list <- list.files(
  path = "../경제원론1_출결/", 
  pattern = "*.xlsx", 
  full.names = TRUE
)

files <- lapply(files_list, read_excel)

for (i in 1:length(files)) {
  if(i %in% c(1, 3, 6, 7, 9, 12, 13, 16, 18, 19, 22, 23, 26, 28, 30)){
    files[[i]] <- files[[i]] %>% 
      mutate(part = 1)
  }else{
    files[[i]] <- files[[i]] %>% 
      mutate(part = 2)
  }
}

files2 <- do.call(bind_rows, files)
colnames(files2) <- c("id", "name", "email", "start", "end", "duration", "part")
files2 <- files2 %>% dplyr::select(-email, -id)

temp <- files2 %>% count(name)

files3 <- files2 %>%
  mutate(
    name = substr(name, 1, 3)
  ) %>%
  filter(name != "Adm")

temp <- files3 %>% count(name)

files4 <- files3 %>%
  mutate(
    start = ymd_hms(start), 
    end = ymd_hms(end)
  )

files5 <- files4 %>%
  mutate(
    date = date(start),
    enter = hms::as_hms(start),
    exit = hms::as_hms(end),
    duration = hms(duration),
    duration2 = period_to_seconds(duration)
  )

files6 <- files5 %>%
  group_by(date, part, name) %>%
  summarise(
    enter = hms::as_hms(min(enter)),
    exit = hms::as_hms(max(exit)),
    time = sum(duration2, na.rm = TRUE) 
  ) %>%
  ungroup 

files7 <- files6 %>%
  group_by(date, part) %>%
  mutate(
    start = enter[name == "강성윤"],
    duration = time[name == "강성윤"]
  ) %>%
  ungroup %>%
  filter(name %in% list2$name)

date <- files7$date %>% unique()
name <- files7$name %>% unique()
data <- expand_grid(name = name, date = date, part = 1:2) 

data2 <- left_join(data, files7, by = c("name", "date", "part")) %>%
  arrange(date, part, name) %>%
  dplyr::select(date, part, name, everything()) %>%
  filter(date != "2021-07-28")

data3 <- data2 %>%
  mutate(
    start_c = start + dminutes(5),
    start_c = hms::as_hms(start_c),
    
    late = if_else(enter > start_c, 1, 0),
    late = if_else(is.na(late), 0, late),
    
    absent = if_else(is.na(duration), 1, 0), 
    
    less = if_else(time < duration * 0.9, 1, 0),
    less = if_else(is.na(less), 0, less)
  ) 

score <- data3 %>%
  group_by(name) %>%
  summarise(
    late = sum(late, na.rm = TRUE), 
    absent = sum(absent, na.rm = TRUE), 
    less = sum(less, na.rm = TRUE)
  ) %>%
  ungroup

score2 <- full_join(list2, score, by = "name") %>%
  arrange(id)

write_xlsx(score2, "경제원론1(21S)_출결결과.xlsx")

write_xlsx(data3, "경제원론1(21S)_출결근거.xlsx")
