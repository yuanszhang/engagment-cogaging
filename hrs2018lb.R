# Proj: social engagement in HRS - 2018 ---

#set directoy
setwd("/Users/yz4510/Dropbox/hrs-soc-eng/soc_eng")

#check current directory 
getwd()

# Load Libraries ----
#install the packages if you haven't
install.packages("haven")

#load packages
library(haven)
library(tidyverse)



# Data Import ----
# define path to the folder where the HRS data are located
hrs18data <- "/Users/yuansz/Documents/DATA/HRS/2018/h18sta"

# specify the name of the data file
data_h18lb <- "h18lb_r.dta"

# combine the folder and file to get the full path
path_h18lb_r <- file.path(hrs18data, data_h18lb)

# read the data into R
h18lb_r <- read_dta(path_h18lb_r)


# Variable Creation ---- 

##engagement in community activities 

### volunterring 
table(h18lb_r$QLB001C)  #tabulation to check the var: use table, and then tell R: dataset$var

h18lb_r <- h18lb_r %>% 
         mutate(qvol = case_when(  #qvol is the new var we are creating 
           QLB001C == 6 | QLB001C == 7 ~ 0,
           QLB001C == 5 | QLB001C == 4 ~ 1, 
           QLB001C == 3 | QLB001C == 2 | QLB001C == 1 ~ 2
           ))

table(h18lb_r$qvol)

#check: please try both approaches
check_vol <- table(h18lb_r$qvol, h18lb_r$QLB001C)

check_vol_1 <- h18lb_r %>% 
    count(qvol, QLB001C)

### charity work 
h18lb_r <- h18lb_r %>% 
  mutate(qcharity = case_when(
    QLB001D == 6 | QLB001D == 7 ~ 0,
    QLB001D == 5 | QLB001D == 4 ~ 1, 
    QLB001D == 3 | QLB001D == 2 | QLB001D == 1 ~ 2
    ))

#check: please try both approaches
check_charity <- table(h18lb_r$qcharity, h18lb_r$QLB001D)

check_charity <- h18lb_r %>% 
  count(qcharity, QLB001D)

### Educatinal courses
h18lb_r <- h18lb_r %>% 
    mutate(qeduc = case_when(
      QLB001E == 6 | QLB001E == 7 ~ 0,
      QLB001E == 5 | QLB001E == 4 ~ 1,
      QLB001E == 3 | QLB001E == 2 | QLB001E == 1 ~ 2
    ))

#check 
check_qeduc <- table(h18lb_r$qeduc, h18lb_r$QLB001E)

check_qeduc <- h18lb_r %>% 
  count(qeduc, QLB001E)

### Sports or social clubs 
h18lb_r <- h18lb_r %>% 
    mutate(qsocial = case_when(
      QLB001F == 6 | QLB001F == 7 ~ 0,
      QLB001F == 5 | QLB001F == 4 ~ 1,
      QLB001F == 3 | QLB001F == 2 | QLB001F == 1 ~ 2
    ))

#check 
check_qsocial <- h18lb_r %>%
  count(qsocial, QLB001F)

### Non religious organizations 
h18lb_r <- h18lb_r %>% 
  mutate(qorgs = case_when(
    QLB001G == 6 | QLB001G == 7 ~ 0,
    QLB001G == 5 | QLB001G == 4 ~ 1,
    QLB001G == 3 | QLB001G == 2 | QLB001G == 1 ~ 2
  ))

check_qorgs <- h18lb_r %>%
    count(qorgs, QLB001G)

## cognitive activities ##

# Reading 
h18lb_r <- h18lb_r %>% 
  mutate(qread = case_when(
    QLB001I == 6 | QLB001I == 7 ~ 0,
    QLB001I == 5 | QLB001I == 4 ~ 1,
    QLB001I == 3 | QLB001I == 2 | QLB001I == 1 ~ 2
  ))

check_qread <- h18lb_r %>% 
    count(qread, QLB001I)

### word games
table(h18lb_r$QLB001K)

h18lb_r <- h18lb_r %>%
    mutate(qword = case_when(
      QLB001K == 6 | QLB001K == 7 ~ 0,
      QLB001K == 5 | QLB001K == 4 ~ 1,
      QLB001K == 3 | QLB001K == 2 | QLB001K == 1 ~ 2
    ))

check_qword <- h18lb_r %>%
    count(qword, QLB001K)

### cards
table(h18lb_r$QLB001L)
h18lb_r <- h18lb_r %>%
  mutate(qcards = case_when(
    QLB001L == 6 | QLB001L == 7 ~ 0,
    QLB001L == 5 | QLB001L == 4 ~ 1,
    QLB001L == 3 | QLB001L == 2 | QLB001L == 1 ~ 2
  ))

check_qcards <- h18lb_r %>% 
    count(qcards, QLB001L)

### writing 
table(h18lb_r$QLB001M)
h18lb_r <- h18lb_r %>%
  mutate(qwrite = case_when(
    QLB001M == 6 | QLB001M == 7 ~ 0,
    QLB001M == 5 | QLB001M == 4 ~ 1,
    QLB001M == 3 | QLB001M == 2 | QLB001M == 1 ~ 2
  ))

check_qwrite <- h18lb_r %>% 
  count(qwrite, QLB001M)


## Home-based creative activities ##

### gardening
table(h18lb_r$QLB001O)
h18lb_r <- h18lb_r %>%
  mutate(qgarden = case_when(
    QLB001O == 6 | QLB001O == 7 ~ 0,
    QLB001O == 5 | QLB001O == 4 ~ 1,
    QLB001O == 3 | QLB001O == 2 | QLB001O == 1 ~ 2
  ))

check_qgarden <- h18lb_r %>% 
  count(qgarden, QLB001O)

### baking or cooking
table(h18lb_r$QLB001P)
h18lb_r <- h18lb_r %>%
  mutate(qbake = case_when(
    QLB001P == 6 | QLB001P == 7 ~ 0,
    QLB001P == 5 | QLB001P == 4 ~ 1,
    QLB001P == 3 | QLB001P == 2 | QLB001P == 1 ~ 2
  ))

check_qbake <- h18lb_r %>% 
  count(qbake, QLB001P)

### crafts 
table(h18lb_r$QLB001Q)

h18lb_r <- h18lb_r %>%
  mutate(qcraft = case_when(
    QLB001Q == 6 | QLB001Q == 7 ~ 0,
    QLB001Q == 5 | QLB001Q == 4 ~ 1,
    QLB001Q == 3 | QLB001Q == 2 | QLB001Q == 1 ~ 2
  ))

check_qcraft<- h18lb_r %>% 
  count(qcraft, QLB001Q)

### hobbies 
table(h18lb_r$QLB001R)

h18lb_r <- h18lb_r %>%
  mutate(qhobby = case_when(
    QLB001R == 6 | QLB001R == 7 ~ 0,
    QLB001R == 5 | QLB001R == 4 ~ 1,
    QLB001R == 3 | QLB001R == 2 | QLB001R == 1 ~ 2
  ))

check_qhobby<- h18lb_r %>% 
  count(qhobby, QLB001R)

## Physical Activities 

### exercise/sports
table(h18lb_r$QLB001S)

h18lb_r <- h18lb_r %>%
  mutate(qexer = case_when(
    QLB001S == 6 | QLB001S == 7 ~ 0,
    QLB001S == 5 | QLB001S == 4 ~ 1,
    QLB001S == 3 | QLB001S == 2 | QLB001S == 1 ~ 2
  ))

check_qexer<- h18lb_r %>% 
  count(qexer, QLB001S)

### walking for 20min
table(h18lb_r$QLB001T)

h18lb_r <- h18lb_r %>%
  mutate(qwalk = case_when(
    QLB001T == 6 | QLB001T == 7 ~ 0,
    QLB001T == 5 | QLB001T == 4 ~ 1,
    QLB001T == 3 | QLB001T == 2 | QLB001T == 1 ~ 2
  ))

check_qwalk<- h18lb_r %>% 
  count(qwalk, QLB001T)


#### Summary score for social enagegment 
h18lb_r$total_QSE <- rowSums(h18lb_r[,c("qvol", "qcharity","qeduc", "qsocial", "qorgs", "qread", "qword", "qcards", "qwrite", "qgarden", "qbake", "qcraft", "qhobby", "qexer", "qwalk")])

table(h18lb_r$total_QSE)
hist(h18lb_r$total_QSE)

# Save the data ---
save(h18lb_r, file = "h18lb_r.RData") #this saves data in your current working directory, use getwd() to check your current working directoy

