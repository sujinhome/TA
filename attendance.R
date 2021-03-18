# ****************************************
# This script merges register file and records of attendance in zoom
#
# Last updated: Sujin Lee, 2021 March
#
# How to use:
#   - Upload register and attendance records from zoom.
#   - copy and paste March 17 version and change the followings: 
#       1) dates: march17, "2021-03-17", "0317"
#       2) participants with their name as an email address
# ****************************************


# install.packages("pacman")

library(pacman)
p_load("tidyverse", "readxl", "magrittr", "stringr", "data.table", "lubridate", "tidylog")


#### List of all students in the class ####

list <- read_excel("G:/공유 드라이브/금융경제세미나/participants_list.xlsx")
# data from class


list <- list[-1]
colnames(list) <- c("major", "id", "name", "type", "email", "hp")

students <- list %>%
  filter(type == "학생") %>%
  dplyr::select(major, id, name)
# filter only students (exclude TA, professor)


#### 3/17 ####
march17 <- fread("G:/공유 드라이브/금융경제세미나/participants_0317.csv")
# zoom records

temp <- march17
colnames(temp) <- c("nick", "email", "attend", "exit", "duration", "geust")

temp %<>% dplyr::select(-email, -geust)

temp %<>%
  mutate(
    nick2 = str_replace(nick, "([가-힣][가-힣]) ([가-힣])", "\\2\\1"),
    # 수진 이 --> 이수진
    nick3 = str_replace(nick2, "([가-힣]) ([가-힣][가-힣])", "\\1\\2"),
    # 이 수진 --> 이수진
    nick4 = str_replace(nick3, "([가-힣]) ([가-힣])", "\\2\\1"), 
    # 찬 허 --> 허찬
    name1 = str_extract(nick4, "[가-힣][가-힣][가-힣]"),
    # 학번, 이름 다 있는 열에서 세 글자 이름 추출
    name2 = str_extract(nick4, "[가-힣][가-힣]"),
    # 학번, 이름 다 있는 열에서 두 글자 이름 추출
    name = ifelse(is.na(name1), name2, name1)
    # 이름이 세 글자가 아니면 두 글자 이름으로.
    ) 

temp %>% filter(is.na(name)) %>% dplyr::select(nick)
# 한글 이름 없는 사람 확인

temp[is.na(temp$name), c("name")] = c("홍길동", "이영희", "김영숙")
# 순서대로 써주기

temp %<>% 
  arrange(name) %>%
  mutate(
    attend = ymd_hms(attend),
    attend = strptime(attend, "%Y-%m-%d %I:%M:%S"),
    exit = ymd_hms(exit),
    exit = strptime(exit, "%Y-%m-%d %I:%M:%S")
    ) %>%
  dplyr::select(name, attend, exit, duration) 
# Keep only the variables in need and convert attend and exit variables to time format

temp %<>% filter(exit >= "2021-03-17 00:59:59")
# 수업 1시 전에 나간 기록은 제외

temp %<>% 
  group_by(name) %>%
  summarise(
    attend = min(attend), 
    exit = max(exit), 
    duration = sum(duration, na.rm = TRUE)
    ) %>%
  ungroup
# 학생별로 가장 이른 접속시간, 가장 나중의 퇴장 시간과 총 접속시간

temp %<>% 
  mutate(
    late = ifelse(attend > "2021-03-17 01:30:00", 1, 0),
    # 첫 입장이 1시 30분 이후면 지각 처리
    pass = ifelse(duration >= 80, 1, 0)
    # 총 80분 이상 수강했으면 출석 처리
    )

colnames(temp) <- paste(colnames(temp), "0317", sep = "_")

students %<>% left_join(., temp, by = c("name" = "name_0317"))
# 출석부 목록에 연결

students$pass_0317[is.na(students$pass_0317)] = 0
# 해당 수업일에 데이터가 없으면 결석 처리

students %<>% 
  dplyr::select(major, id, name, starts_with("pass"), starts_with("late"), everything())
