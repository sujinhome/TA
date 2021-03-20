# ****************************************
# This R script is for checking if students handed in their assignmnet
#
# Last updated: Sujin Lee, 2021 March
#
# 이용 방법:
#   - eTL에서 과제 전체를 다운 받은 후 금융경제세미나 폴더에 "hw_due(날짜)"를 만들어 여기에 압출 풀기  
#   - 데이터 불러들이는 경로는 각자 추가하면 됨.
#   - 3월 17일 version을 복붙 후 다음 사항들 변경;
#        1) 날짜 관련 march17, "2021-03-17", "0317"
#        2) 과제 제출 대상 조 바꾸기
#        3) 면제자 이름 변경 
# ****************************************

# install.packages("pacman")
library(pacman)
p_load("tidylog", "tidyverse", "readxl", "magrittr", "stringr", "data.table", "lubridate", "writexl")


#### 전체 학생 목록 불러오기 ####

list <- read_excel("G:/공유 드라이브/금융경제세미나/participants_list.xlsx")
# 이수진, 김형석 경로

list <- list[-1]
colnames(list) <- c("major", "id", "name", "type", "email", "hp")

students <- list %>%
  filter(type == "학생") %>%
  dplyr::select(major, id, name)
# 학생만 filter. (조교, 교수 등 제외)

students %<>% 
  mutate(
    grp = case_when(
      id >= "2012-10246" & id <= "2015-16499" ~ 1,
      id >= "2015-17074" & id <= "2016-17675" ~ 2,
      id >= "2016-18836" & id <= "2017-15863" ~ 3,
      id >= "2017-15950" & id <= "2018-18073" ~ 4,
      id >= "2018-18100" & id <= "2021-90349" ~ 5,
      TRUE ~ NA_real_)
  )
# 과제 조는 "grp"


##### due 3/17 ####

hw <- list.files("G:/공유 드라이브/금융경제세미나/hw_due0317/")
# eTL에서 받은 파일 안의 파일명 불러오기

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
      name %in% c("윤현지", "백창하", "이동연", "노서희", "백지훈", "이호준", "송기우", "박도윤", "최재혁", "김성균", "염세은", "고경연", "한준희", "남수현") ~ "면제",
      # 면제자
      TRUE ~ as.character(due_0317)
    )
  )

write_xlsx(students, "G:/공유 드라이브/금융경제세미나/hw_due0317.xlsx")
# hw_due0317.xlsx에서 과제 평가 후 score_0317에 점수 기입 후 hw_due0317_scored.xlsx로 저장하기


#### due 3/24 ####

hw <- list.files("G:/공유 드라이브/금융경제세미나/hw_due0324/")

hw2 <- str_split_fixed(hw, "_", n = 3)[, 1:2]

colnames(hw2) <- c("name", "id")

hw2 %<>% as.data.frame() %>%
  mutate(
    due_0324 = 0, 
    score_0324 = NA
  )

students %<>% left_join(., hw2, by = c("id", "name"))

students %<>% 
  mutate(
    due_0324 = case_when(
      grp %in% c(1, 4) & is.na(due_0324) ~ 1, 
      TRUE ~ as.numeric(due_0317)
    ),
    due_0324 = case_when(
      grp %in% c(1, 4) & name %in% c("김원재", "염세은", "이호준", "조항준", "백창하", "고태형", "차동경", "최재혁", "백지훈", "윤현지", "최현재", "김재현", "남수연", "이보연", "강진구", "강성한", "최선", "송기우") ~ 0,
      TRUE ~ as.numeric(due_0324)
    )
  )

write_xlsx(students, "G:/공유 드라이브/금융경제세미나/hw_due0324.xlsx")







#### 평가한 과제 점수 파일들 합치기 ####

score <- list.files(path = "G:/공유 드라이브/금융경제세미나/", pattern = "_scored.xlsx$")



