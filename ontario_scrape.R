# ******
# Variants of concern PDF scrape
# as of April 19, 2021
# always check page numbers haven't been changed

# load libraries
library(tidyverse)
library(pdftools)
library(XML)

#create automated url
#only part of url that changes is the date so use Sys.Date function
str1 <- "https://files.ontario.ca/moh-covid-19-report-en-"
str2 <- Sys.Date()
str3 <- ".pdf"
url <- paste0(str1,str2,str3)

# download the pdf to working directory and change name
download.file(url, 'Daily_VOC_Report.pdf')

# read the pdf
pdf <- pdf_text("Daily_VOC_Report.pdf") %>% 
  strsplit(split = "\n")

# **************
#page27 cleaning
page27 <- pdf[[27]]
cleanPage27<- page27[-c(1:5,10,13,16,
                        19,23,26,29,
                        32,36,40,41)]
cleanPage27 <- cleanPage27[-c(3,5,7,9,
                              12,14,16,18,21,24)]
cleanPage27 <- str_replace_all(cleanPage27, "([A-z])", "")
cleanPage27 <- str_replace_all(cleanPage27, "([&])", "")
cleanPage27 <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "",cleanPage27))
cleanPage27 <- reshape::colsplit(cleanPage27, "\\,", names=c("day1", "day2", "day3", "day4",
                                                             "day5", "day6", "day7"))

#page28 cleaning
page28 <- pdf[[28]]
cleanPage28<- page28[-c(1:4,6,7,9,10,12,
                        16,18,20,22,
                        23,25,26,28,29,31,32,34,35,37,40)]
cleanPage28 <- str_replace_all(cleanPage28, "([A-z])", "")
cleanPage28 <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "",cleanPage28))
cleanPage28 <- reshape::colsplit(cleanPage28, "\\,", names=c("day1", "day2", "day3", "day4",
                                                             "day5", "day6", "day7"))

#page29 cleaning
page29 <- pdf[[29]]
cleanPage29<- page29[-c(1:4,6,7,9,11:20)]
cleanPage29 <- str_replace_all(cleanPage29, "([A-z])", "")
cleanPage29 <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "",cleanPage29))
cleanPage29 <- reshape::colsplit(cleanPage29, "\\,", names=c("day1", "day2", "day3", "day4",
                                                             "day5", "day6", "day7"))


# combine pages
allPages <- bind_rows(cleanPage27, cleanPage28, cleanPage29) 

# create final data frame that includes health regions
# read in health region names and codes
variantData <- read_csv("health_regions.csv") %>% 
  bind_cols(allPages) %>% 
  mutate(location = case_when(
    health_region_name == "Algoma Public Health Unit" ~ "n",
    health_region_name == "North Bay Parry Sound District Health Unit" ~ "n",
    health_region_name == "Northwestern Health Unit" ~ "n",
    health_region_name == "Porcupine Health Unit" ~ "n",
    health_region_name == "Sudbury and District Health Unit" ~ "n",
    health_region_name == "Thunder Bay District Health Unit" ~ "n",
    health_region_name == "Timiskaming Health Unit" ~ "n",
    TRUE ~ "s"
  )) %>% 
  select(health_region_name, location, health_region_code, day7) %>% 
  arrange(-desc(health_region_name)) 
View(variantData)

# Write file to csv
write.csv(variantData,
          file = "VOC%_PHU.csv",
          row.names = F)




