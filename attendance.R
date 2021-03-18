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

list <- read_excel("G:/°øÀ¯ µå¶óÀÌºê/±İÀ¶°æÁ¦¼¼¹Ì³ª/participants_list.xlsx")
# data from class


list <- list[-1]
colnames(list) <- c("major", "id", "name", "type", "email", "hp")

students <- list %>%
  filter(type == "ÇĞ»ı") %>%
  dplyr::select(major, id, name)
# filter only students (exclude TA, professor)


#### 3/17 ####
march17 <- fread("G:/°øÀ¯ µå¶óÀÌºê/±İÀ¶°æÁ¦¼¼¹Ì³ª/participants_0317.csv")
# zoom records

temp <- march17
colnames(temp) <- c("nick", "email", "attend", "exit", "duration", "geust")

temp %<>% dplyr::select(-email, -geust)

temp %<>%
  mutate(
    nick2 = str_replace(nick, "([°¡-ÆR][°¡-ÆR]) ([°¡-ÆR])", "\\2\\1"),
    # ¼öÁø ÀÌ --> ÀÌ¼öÁø
    nick3 = str_replace(nick2, "([°¡-ÆR]) ([°¡-ÆR][°¡-ÆR])", "\\1\\2"),
    # ÀÌ ¼öÁø --> ÀÌ¼öÁø
    nick4 = str_replace(nick3, "([°¡-ÆR]) ([°¡-ÆR])", "\\2\\1"), 
    # Âù Çã --> ÇãÂù
    name1 = str_extract(nick4, "[°¡-ÆR][°¡-ÆR][°¡-ÆR]"),
    # ÇĞ¹ø, ÀÌ¸§ ´Ù ÀÖ´Â ¿­¿¡¼­ ¼¼ ±ÛÀÚ ÀÌ¸§ ÃßÃâ
    name2 = str_extract(nick4, "[°¡-ÆR][°¡-ÆR]"),
    # ÇĞ¹ø, ÀÌ¸§ ´Ù ÀÖ´Â ¿­¿¡¼­ µÎ ±ÛÀÚ ÀÌ¸§ ÃßÃâ
    name = ifelse(is.na(name1), name2, name1)
    # ÀÌ¸§ÀÌ ¼¼ ±ÛÀÚ°¡ ¾Æ´Ï¸é µÎ ±ÛÀÚ ÀÌ¸§À¸·Î.
    ) 

temp %>% filter(is.na(name)) %>% dplyr::select(nick)
# ÇÑ±Û ÀÌ¸§ ¾ø´Â »ç¶÷ È®ÀÎ

temp[is.na(temp$name), c("name")] = c("È«±æµ¿", "ÀÌ¿µÈñ", "±è¿µ¼÷")
# ¼ø¼­´ë·Î ½áÁÖ±â

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
# ¼ö¾÷ 1½Ã Àü¿¡ ³ª°£ ±â·ÏÀº Á¦¿Ü

temp %<>% 
  group_by(name) %>%
  summarise(
    attend = min(attend), 
    exit = max(exit), 
    duration = sum(duration, na.rm = TRUE)
    ) %>%
  ungroup
# ÇĞ»ıº°·Î °¡Àå ÀÌ¸¥ Á¢¼Ó½Ã°£, °¡Àå ³ªÁßÀÇ ÅğÀå ½Ã°£°ú ÃÑ Á¢¼Ó½Ã°£

temp %<>% 
  mutate(
    late = ifelse(attend > "2021-03-17 01:30:00", 1, 0),
    # Ã¹ ÀÔÀåÀÌ 1½Ã 30ºĞ ÀÌÈÄ¸é Áö°¢ Ã³¸®
    pass = ifelse(duration >= 80, 1, 0)
    # ÃÑ 80ºĞ ÀÌ»ó ¼ö°­ÇßÀ¸¸é Ãâ¼® Ã³¸®
    )

colnames(temp) <- paste(colnames(temp), "0317", sep = "_")

students %<>% left_join(., temp, by = c("name" = "name_0317"))
# Ãâ¼®ºÎ ¸ñ·Ï¿¡ ¿¬°á

students$pass_0317[is.na(students$pass_0317)] = 0
# ÇØ´ç ¼ö¾÷ÀÏ¿¡ µ¥ÀÌÅÍ°¡ ¾øÀ¸¸é °á¼® Ã³¸®

students %<>% 
  dplyr::select(major, id, name, starts_with("pass"), starts_with("late"), everything())
