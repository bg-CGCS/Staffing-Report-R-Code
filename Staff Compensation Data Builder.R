#Clear environment
rm(list=ls())

#Libraries
library(dplyr)
library(tidyverse)
library(scales)
library(haven)
library(stringr)
library(readxl)    #Used to read in historic excel files

#Set year 
yearx <- "2425"

#CGCS District Reference File 
cgcs_leaid <- read.csv(paste0("C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Custom LEAIDs by Year/CGCS Districts LEAID reference ",yearx,".csv"))

cgcs_leaid_slim <- read.csv("C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/CGCS_LEAID_SLIM.csv")

todays_date <- strftime(Sys.Date(), "%Y%m%d")

#Functions
#functions for reading in the different files.
read.csv.staff <- function(yearx){
  read.csv(paste0("C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Staff Data/",yearx,".csv"))
}

read.state.csv.staff <- function(yearx){
  read.csv(paste0("C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Staff Data/",yearx,"_state.csv"))
}

#Function to read in the student enrollment CCD file
read.csv.grs <- function(yearx){
  read.csv(paste0("C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Grade.Race.Sex/",yearx,".csv"))
}

#Function for reading in compensation data
read.csv.money <- function(yearx){
  read_sas(paste0("C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Compensation/",yearx,".SAS7BDAT"))
}

#saves the cleaned file as a csv to the completed folder
save.csv <- function(filex, titlex) {
  write_excel_csv(filex, paste0("C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/R Code/Completed Reports/",yearx,"/",titlex,".csv"))
  
}

#Section for creating district compensation comparisons data. 
money.raw <- read.csv.money(yearx)

money.raw$Z35  <- ifelse(money.raw$Z35 < 0, 0, money.raw$Z35)
money.raw$Z36 <- ifelse(money.raw$Z36 < 0, 0, money.raw$Z36)
money.raw$Z37 <- ifelse(money.raw$Z37 < 0, 0, money.raw$Z37)
money.raw$Z38 <- ifelse(money.raw$Z38 < 0, 0, money.raw$Z38)

money.teachers <- money.raw %>%
  subset(select = c("YEAR", "LEAID", "NAME", "Z35","Z36","Z37","Z38")) %>%
  mutate("Total_Teacher_Salary" = Z35 + Z36 + Z37 + Z38) %>%
  rename("LEA_NAME" = "NAME")

money.teachers$LEAID <- as.integer(money.teachers$LEAID)

state.staff.abv <- Staff.All.Slim.cgcsID %>%
  subset(select = c("LEAID","Total_Enrollment", "Pre-kindergarten Teachers","Kindergarten Teachers","Elementary Teachers","Secondary Teachers","Ungraded Teachers"))

money.teach.staff <- money.teachers %>%
  left_join(state.staff.abv, by = c("LEAID")) %>%
  subset(select = c("YEAR", "LEAID", "LEA_NAME", "Z35","Z36","Z37","Z38","Total_Teacher_Salary", "Total_Enrollment", 
                    "Pre-kindergarten Teachers","Kindergarten Teachers","Elementary Teachers","Secondary Teachers","Ungraded Teachers"))


#Pulling out council districts to merge NY and NOLA into single district rows. 
money.cgcs.1 <- money.teach.staff %>%
  left_join(cgcs_leaid, by = c("LEAID")) %>%
  mutate(cgcsflag = if_else(!is.na(District_Name) , 1, 0)) %>%
  filter(cgcsflag == "1")

names(money.cgcs.1)

money.cgcs.2 <- money.cgcs.1 %>%
  group_by(YEAR, District_Name) %>%
  summarise_at(c("Z35","Z36","Z37","Z38","Total_Teacher_Salary", "Total_Enrollment", 
                 "Pre-kindergarten Teachers","Kindergarten Teachers","Elementary Teachers",
                 "Secondary Teachers","Ungraded Teachers"),  sum, na.rm = TRUE)

money.cgcs.final <- money.cgcs.2 %>%
  left_join(cgcs_leaid_slim, by = "District_Name")%>%
  rename("LEA_NAME" = "District_Name")

non.cgcs <- money.teach.staff %>%
  left_join(cgcs_leaid, by = c("LEAID")) %>%
  mutate(cgcsflag = if_else(!is.na(District_Name) , 1, 0)) %>%
  filter(cgcsflag == "0") %>%
  subset(select = c("YEAR", "LEAID", "LEA_NAME", "Z35","Z36","Z37","Z38","Total_Teacher_Salary", "Total_Enrollment", 
                    "Pre-kindergarten Teachers","Kindergarten Teachers","Elementary Teachers",
                    "Secondary Teachers","Ungraded Teachers"))

all.staff.slim.final <- non.cgcs %>%
  rbind(money.cgcs.final)


#Section for creating compensation comparisons data. 
money.raw <- read.csv.money(yearx)

money.raw$E11A <- ifelse(money.raw$E11A < 0, 0, money.raw$E11A)
money.raw$E11B <- ifelse(money.raw$E11B < 0, 0, money.raw$E11B)
money.raw$E11C <- ifelse(money.raw$E11C < 0, 0, money.raw$E11C)
money.raw$E11D <- ifelse(money.raw$E11D < 0, 0, money.raw$E11D)

money.raw$FIPS = str_remove(money.raw$FIPS , "^0+")

money.teachers <- money.raw %>%
  subset(select = c("SURVYEAR", "FIPS", "STNAME", "E11A","E11B","E11C","E11D")) %>%
  mutate("Total_Teacher_Salary" = E11A + E11B + E11C + E11D) %>%
  rename("STATENAME" = "STNAME") %>%
  rename("FIPST" = "FIPS")

money.teachers$FIPST <- as.integer(money.teachers$FIPST)

state.staff.abv <- State.staff.enrollment.2223 %>%
  subset(select = c("FIPST.x","STUDENT_COUNT", "Pre-kindergarten Teachers","Kindergarten Teachers","Elementary Teachers","Secondary Teachers","Ungraded Teachers"))

money.teach.staff <- money.teachers %>%
  left_join(state.staff.abv, by = c("FIPST" = "FIPST.x")) %>%
  subset(select = c("SURVYEAR", "FIPST", "STATENAME", "E11A","E11B","E11C","E11D","Total_Teacher_Salary", "STUDENT_COUNT", 
                    "Pre-kindergarten Teachers","Kindergarten Teachers","Elementary Teachers","Secondary Teachers","Ungraded Teachers"))


save.csv(money.teach.staff, paste0("Teacher_Compensation_State",yearx,"_",todays_date))

save.csv(all.staff.slim.final, paste0("Teacher_Compensation_District",yearx,"_",todays_date))

