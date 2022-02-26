library(tidyverse)
library(purrr)
library(lubridate)
library(vtable)
library(car)
library(jtools)
library(fixest)
library(wooldridge)
library(stringr)
library(vtable)
library(plm)

setwd(getwd())
list_files <- list.files(path = 'Data Exploration Task Ryan Cowles/Data/', pattern = 'trends_up_to_')
id_name_link <- read.csv(file = 'Data Exploration Task Ryan Cowles/Data/id_name_link.csv')
college_scorecard <- read.csv(file = 'Data Exploration Task Ryan Cowles/Data/Most+Recent+Cohorts+(Scorecard+Elements).csv')

for(name in list_files) {
  new_data <- read.csv(file = paste('Data Exploration Task Ryan Cowles/Data/', name, sep =""))
  #new_data <- new_data %>% filter(!is.na(schid), !is.na(schname), !is.na(keynum), !is.na(monthorweek), !is.na(index))
  new_data[1] <- apply(new_data[1], 2, function(x) as.numeric(as.character(x)))
  if (name == list_files[1]) {
    google_trends <- new_data
  } else {
    google_trends <- google_trends %>% bind_rows(new_data)
  }
}

# remove colleges that have the same name
id_name_link <- id_name_link %>% 
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1)

# create date columns
google_trends <- google_trends %>% 
  mutate(date = str_sub(monthorweek, 1, 10)) %>%
  mutate(date = ymd(date)) 

# standardize index
google_trends1 <- google_trends %>%
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index, na.rm = TRUE))/(sd(index, na.rm = TRUE)))

# Sum indexes for schools by week
google_trends100 <- google_trends1 %>%
  group_by(schname, date) %>%
  summarize(index_std_mean = mean(index_std, na.rm = TRUE))
  
#google_trends100 <- google_trends100 %>% 
  #filter(date >= ymd('2015-08-30')) %>% mutate(month = month(date), year = year(date))

# Join with id name link
google_trends2 <- google_trends100 %>% 
  left_join(id_name_link) %>%
  mutate(UNITID = unitid, OPEID = opeid, scorecard_available = date >= ymd('2015-10-04')) #2015-08-30  2015-09-27 2015-10-04

# Join with college scorecard
cleaned_data <- google_trends2 %>% 
  left_join(college_scorecard) %>%
  filter(PREDDEG == 3)

Z <- c(which(colnames(cleaned_data)=="md_earn_wne_p10.REPORTED.EARNINGS"))
cleaned_data[ , Z] <- apply(cleaned_data[ , Z], 2, function(x) as.numeric(as.character(x)))

H <- cleaned_data %>%
  group_by(schname) %>%
  summarize(median = median(md_earn_wne_p10.REPORTED.EARNINGS), na.rm = TRUE)

cleaned_data <- cleaned_data %>%
  unite(AVG_COST_ANNUAL, c("NPT4_PUB.AVERAGE.ANNUAL.COST", "NPT4_PRIV"), sep ="") %>%
  mutate_at("AVG_COST_ANNUAL", str_replace, "NULL", "") %>%
  unite(NPT41, c("NPT41_PUB", "NPT41_PRIV"), sep = "") %>%
  mutate_at("NPT41", str_replace, "NULL", "") %>%
  unite(NPT42, c("NPT42_PUB", "NPT42_PRIV"), sep = "") %>%
  mutate_at("NPT42", str_replace, "NULL", "") %>%
  unite(NPT43, c("NPT43_PUB", "NPT43_PRIV"), sep = "") %>%
  mutate_at("NPT43", str_replace, "NULL", "") %>%
  unite(NPT44, c("NPT44_PUB", "NPT44_PRIV"), sep = "") %>%
  mutate_at("NPT44", str_replace, "NULL", "") %>%
  unite(NPT45, c("NPT45_PUB", "NPT45_PRIV"), sep = "") %>%
  mutate_at("NPT45", str_replace, "NULL", "") 
  

cleaned_data <- cleaned_data %>% filter(PREDDEG == 3) %>% 
  mutate(median_earnings_10 = md_earn_wne_p10.REPORTED.EARNINGS)

cleaned_data2 <- cleaned_data %>%
  group_by(schname, date) %>% 
  summarize(UNITID, OPEID,  index_std_mean, median_earnings_10, STABBR, LOCALE, scorecard_available, 
            AVG_COST_ANNUAL, GRAD_DEBT_MDN_SUPP, GRAD_DEBT_MDN10YR_SUPP, PCTPELL, UGDS, PPTUG_EF,
            PCIP01,	PCIP03,	PCIP04,	PCIP05,	PCIP09,	PCIP10,	PCIP11,	PCIP12,	PCIP13,	PCIP14,	PCIP15,	
            PCIP16,	PCIP19,	PCIP22,	PCIP23,	PCIP24,	PCIP25,	PCIP26,	PCIP27,	PCIP29,	PCIP30,	PCIP31,	
            PCIP38,	PCIP39,	PCIP40,	PCIP41,	PCIP42,	PCIP43,	PCIP44,	PCIP45,	PCIP46,	PCIP47,	PCIP48,	
            PCIP49,	PCIP50,	PCIP51,	PCIP52,	PCIP54, CONTROL, HBCU,	PBI,	ANNHI,	TRIBAL,	AANAPII,	
            HSI,	NANTI,	MENONLY,	WOMENONLY, RET_FT4, PCTFLOAN, UGDS_WHITE,	UGDS_BLACK,	UGDS_HISP, UGDS_ASIAN,
            NPT41,	NPT42,	NPT43,	NPT44,	NPT45, SAT_AVG)

B <- c(which(colnames(cleaned_data2)=="median_earnings_10"), which(colnames(cleaned_data2)=="PCTPELL"), 
       which(colnames(cleaned_data2)=="AVG_COST_ANNUAL"),
       which(colnames(cleaned_data2)=="GRAD_DEBT_MDN_SUPP"),
       which(colnames(cleaned_data2)=="GRAD_DEBT_MDN10YR_SUPP"),
       which(colnames(cleaned_data2)=="UGDS"),
       which(colnames(cleaned_data2)=="PPTUG_EF"), 16:53, which(colnames(cleaned_data2)=="RET_FT4"), 
       which(colnames(cleaned_data2)=="PCTFLOAN"), which(colnames(cleaned_data2)=="UGDS_WHITE"), 
       which(colnames(cleaned_data2)=="UGDS_BLACK"), which(colnames(cleaned_data2)=="UGDS_HISP"),
       which(colnames(cleaned_data2)=="UGDS_ASIAN"), 70:74, which(colnames(cleaned_data2)=="SAT_AVG"))##############################

C <- c(16:53, which(colnames(cleaned_data2)=="RET_FT4"), which(colnames(cleaned_data2)=="PCTFLOAN"), 
       which(colnames(cleaned_data2)=="PPTUG_EF"), which(colnames(cleaned_data2)=="PCTPELL"),
       which(colnames(cleaned_data2)=="UGDS_WHITE"), 
       which(colnames(cleaned_data2)=="UGDS_BLACK"), which(colnames(cleaned_data2)=="UGDS_HISP"),
       which(colnames(cleaned_data2)=="UGDS_ASIAN"))

D <- c(which(colnames(cleaned_data2)=="CONTROL"))

cleaned_data2[D] <- apply(cleaned_data2[D], 2, function(x) as.character(as.numeric(x)))
       
cleaned_data2[ , B] <- apply(cleaned_data2[ , B], 2, function(x) as.numeric(as.character(x)))

cleaned_data2[ , C] <- apply(cleaned_data2[ , C], 2, function(x) (100*(x)))

cleaned_data2 <- make.pbalanced(cleaned_data2, balance.type = "shared.times")

cleaned_data_test <- cleaned_data2 %>%
  group_by(schname) %>%
  mutate(n = n())

cleaned_data_test2 <- cleaned_data2 %>%
  group_by(schname) %>%
  summarize(cost = median(AVG_COST_ANNUAL), debt = median(GRAD_DEBT_MDN_SUPP),
            sat = median(SAT_AVG), pell = median(PCTPELL), loan = median(PCTFLOAN), diversity = median(UGDS_WHITE), na.rm = TRUE)

cleaned_data_final <- cleaned_data2 %>%
  mutate(earnings = case_when(
    median_earnings_10 < quantile(H$median, 0.5, na.rm = TRUE) ~ 'low',
    median_earnings_10 > quantile(H$median, 0.5, na.rm = TRUE) ~ 'high'
  )) %>%
  mutate(cost = case_when(
    AVG_COST_ANNUAL > quantile(cleaned_data_test2$cost, .5, na.rm = TRUE) ~ 'high.cost',
    AVG_COST_ANNUAL < quantile(cleaned_data_test2$cost, .5, na.rm = TRUE) ~ 'low.cost'
  )) %>%
  mutate(debt = case_when(
    GRAD_DEBT_MDN_SUPP > quantile(cleaned_data_test2$debt, .5, na.rm = TRUE) ~ 'high.debt',
    GRAD_DEBT_MDN_SUPP < quantile(cleaned_data_test2$debt, .5, na.rm = TRUE) ~ 'low.debt'
  )) %>%
  mutate(sat = case_when(
    SAT_AVG > quantile(cleaned_data_test2$sat, .5, na.rm = TRUE) ~ 'high.sat',
    SAT_AVG < quantile(cleaned_data_test2$sat, .5, na.rm = TRUE) ~ 'low.sat'
  )) %>%
  mutate(pell = case_when(
    PCTPELL > quantile(cleaned_data_test2$pell, .5, na.rm = TRUE) ~ 'high.pell',
    PCTPELL < quantile(cleaned_data_test2$pell, .5, na.rm = TRUE) ~ 'low.pell'
  )) %>%
  mutate(loan = case_when(
    PCTFLOAN > quantile(cleaned_data_test2$loan, .5, na.rm = TRUE) ~ 'high.loan',
    PCTFLOAN < quantile(cleaned_data_test2$loan, .5, na.rm = TRUE) ~ 'low.loan'
  ))
  
write.csv(cleaned_data_final, "Data Exploration Task Ryan Cowles/Data/cleaned_data_final.csv")



