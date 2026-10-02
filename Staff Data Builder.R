#Clear environment
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(scales)
library(haven)
library(stringr)

#Set year
yearx <- "2425"

#CGCS District Reference File
cgcs_leaid <- read.csv(
  paste0(
    "C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Custom LEAIDs by Year/CGCS Districts LEAID reference ",
    yearx,
    ".csv"
  )
)

cgcs_leaid_slim <- read.csv(
  "C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/CGCS_LEAID_SLIM.csv"
)

todays_date <- strftime(Sys.Date(), "%Y%m%d")

#Functions
#functions for reading in the different files. for EL & SWD the files are csv for 2021 and after
read.csv.staff <- function(yearx) {
  read.csv(
    paste0(
      "C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Staff Data/",
      yearx,
      ".csv"
    )
  )
}

#Function to read in the student enrollment CCD file
read.csv.grs <- function(yearx) {
  read.csv(
    paste0(
      "C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Grade.Race.Sex/",
      yearx,
      ".csv"
    )
  )
}

#saves the cleaned file as a csv to the completed folder
save.csv <- function(filex, titlex) {
  write_excel_csv(
    filex,
    paste0(
      "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/R Code/Completed Reports/",
      yearx,
      "/",
      titlex,
      ".csv"
    )
  )
  
}

#Create  a total enrollment calculation to join to the staffing data.
df.grs.raw <- read.csv.grs(yearx)

summary(df.grs.raw$STUDENT_COUNT)

df.grs.raw$LEAID <- as.integer(df.grs.raw$LEAID)

df.grs.1 <- df.grs.raw %>%
  left_join(cgcs_leaid, by = c("LEAID")) %>%
  mutate(cgcsflag = if_else(!is.na(District_Name) , 1, 0))

#need to filter total-indicator for just Subtotal 4 to get a K12 and PK12 total along with ED Unit Total to get the total enrollment.
#Also filter for dms flag != derived
df.grs.2 <- df.grs.1 %>%
  filter(TOTAL_INDICATOR == "Subtotal 4 - By Grade" |
           TOTAL_INDICATOR == "Education Unit Total") %>%
  filter(DMS_FLAG != "Derived") %>%
  filter(
    GRADE != "Not Specified" &
      GRADE != "Ungraded" &
      GRADE != "Adult Education" &
      GRADE != "Grade 13"
  ) %>%
  subset(
    select = c(
      SCHOOL_YEAR,
      LEAID,
      LEA_NAME,
      District_Name,
      STATENAME,
      GRADE,
      RACE_ETHNICITY,
      SEX,
      STUDENT_COUNT,
      TOTAL_INDICATOR,
      DMS_FLAG,
      cgcsflag
    )
  ) %>%
  rename("COUNT" = "STUDENT_COUNT", "RACE" = "RACE_ETHNICITY")

#Pulling out CGCS districts and summing NY and NOLA
#Filters for CGCS
with.cgcs <- df.grs.2 %>%
  filter(cgcsflag == 1)
#Combines districts by name
with.cgcs.1 <- with.cgcs %>%
  group_by(SCHOOL_YEAR,
           District_Name,
           STATENAME,
           GRADE,
           RACE,
           SEX,
           TOTAL_INDICATOR) %>%
  summarise(COUNT = sum(COUNT , na.rm = TRUE))
#Need to re-add LEAID
with.cgcs.2 <- with.cgcs.1 %>%
  left_join(cgcs_leaid_slim, by = "District_Name") %>%
  select(SCHOOL_YEAR,
         LEAID,
         District_Name,
         STATENAME,
         GRADE,
         RACE,
         SEX,
         COUNT,
         TOTAL_INDICATOR) %>%
  rename("LEA_NAME" = "District_Name")

#Need to rejoin with non CGCS districts.
w.o.CGCS <- df.grs.2 %>%
  filter(cgcsflag == 0) %>%
  select(
    "SCHOOL_YEAR",
    "LEAID",
    "LEA_NAME",
    "STATENAME",
    "GRADE",
    "RACE",
    "SEX",
    "COUNT",
    "TOTAL_INDICATOR"
  )

all.district.data <- with.cgcs.2 %>%
  rbind(w.o.CGCS)

#Creating a pk12 & K12 total enrollment and PK12 & k12 total
#reported Education Unit Total includes AE, ungraded & GR13
#total columns should be 9

PK.12.enrollment <- all.district.data %>%
  filter(TOTAL_INDICATOR == "Subtotal 4 - By Grade") %>%
  group_by(SCHOOL_YEAR, LEAID, LEA_NAME, STATENAME, TOTAL_INDICATOR) %>%
  summarise("COUNT" = sum(COUNT, na.rm = TRUE))

PK.12.enrollment$GRADE <- c("PK12")
PK.12.enrollment$RACE <- c("ALL.RACES")
PK.12.enrollment$SEX <- c("ALL.GENDERS")

K.12.enrollment <- all.district.data %>%
  filter(TOTAL_INDICATOR == "Subtotal 4 - By Grade") %>%
  filter(GRADE != "Pre-Kindergarten") %>%
  group_by(SCHOOL_YEAR, LEAID, LEA_NAME, STATENAME, TOTAL_INDICATOR) %>%
  summarise("COUNT"  = sum(COUNT, na.rm = TRUE))

K.12.enrollment$GRADE <- c("K12")
K.12.enrollment$RACE <- c("ALL.RACES")
K.12.enrollment$SEX <- c("ALL.GENDERS")

total.enrollment <- all.district.data %>%
  rbind(PK.12.enrollment) %>%
  rbind(K.12.enrollment)

#staff data
#pulls in CCD staff file for identified year.
staff.raw <- read.csv.staff(yearx)

names(staff.raw)
unique(staff.raw$STAFF)

staff.raw$LEAID <- as.integer(staff.raw$LEAID)

all.staff.slim <- staff.raw %>%
  filter(DMS_FLAG == "Reported" |
           DMS_FLAG == "Derived")
#filter(STAFF == "LEA Administrators" |
#       STAFF == "LEA Administrative Support Staff"|
#       STAFF == "School administrators"|
#       STAFF == "School Administrative Support Staff"|
#       STAFF == "No Category Codes" |
#       STAFF == "Paraprofessionals/Instructional Aides" |
#       STAFF =="Instructional Coordinators and Supervisors to the Staff")

teachers.1 <- staff.raw %>%
  filter(DMS_FLAG == "Reported" |
           DMS_FLAG == "Derived") %>%
  filter(
    STAFF == "Elementary Teachers" |
      STAFF == "Kindergarten Teachers" |
      STAFF == "Secondary Teachers" |
      STAFF == "Ungraded Teachers"
  )
teachers.2 <- teachers.1 %>%
  group_by(
    SCHOOL_YEAR,
    FIPST,
    STATENAME,
    ST,
    LEA_NAME,
    STATE_AGENCY_NO,
    UNION,
    ST_LEAID,
    LEAID,
    TOTAL_INDICATOR,
    DMS_FLAG
  ) %>%
  summarise(STAFF_COUNT = sum(STAFF_COUNT, na.rm = TRUE))

teachers.2$STAFF <- c("K12_Teachers")

all.staff.slim.edited <- all.staff.slim %>%
  rbind(teachers.2)

#pull out ny and nola to roll-up and re-add
all.staff.slim.edited$LEAID <- as.integer(all.staff.slim.edited$LEAID)

cgcs.1 <- all.staff.slim.edited %>%
  left_join(cgcs_leaid, by = c("LEAID")) %>%
  mutate(cgcsflag = if_else(!is.na(District_Name) , 1, 0)) %>%
  filter(cgcsflag == "1")

names(cgcs.1)
unique(cgcs.1$STAFF)

cgcs.2 <- cgcs.1 %>%
  group_by(SCHOOL_YEAR, STATENAME, STAFF, District_Name) %>%
  summarise(STAFF_COUNT = sum(STAFF_COUNT, na.rm = TRUE))

cgcs.all.staff.slim <- cgcs.2 %>%
  left_join(cgcs_leaid_slim, by = "District_Name") %>%
  rename("LEA_NAME" = "District_Name")


non.cgcs <- all.staff.slim.edited %>%
  left_join(cgcs_leaid, by = c("LEAID")) %>%
  mutate(cgcsflag = if_else(!is.na(District_Name) , 1, 0)) %>%
  filter(cgcsflag == "0") %>%
  subset(select = c(SCHOOL_YEAR, STATENAME, LEAID, LEA_NAME, STAFF, STAFF_COUNT))

all.staff.slim.final <- non.cgcs %>%
  rbind(cgcs.all.staff.slim) %>%
  pivot_wider(names_from = STAFF, values_from = STAFF_COUNT)

#Add the total enrollment to the staff data. calculations are performed in the Excel sheet.
member <- total.enrollment %>%
  filter(GRADE == "K12" &
           RACE == "ALL.RACES" &
           SEX == "ALL.GENDERS") %>%
  pivot_wider(names_from = GRADE, values_from = COUNT) %>%
  subset(select = c(LEAID, K12))

Staff.All.Slim <- all.staff.slim.final %>%
  left_join(member, by = c("LEAID"))

EUT.member <- total.enrollment %>%
  filter(TOTAL_INDICATOR == "Education Unit Total") %>%
  pivot_wider(names_from = GRADE, values_from = COUNT) %>%
  rename("Total_Enrollment" = "No Category Codes") %>%
  subset(select = c(LEAID, Total_Enrollment))

Staff.All.Slim.2 <- Staff.All.Slim %>%
  left_join(EUT.member, by = c("LEAID"))

Staff.All.Slim.cgcsID <- Staff.All.Slim.2 %>%
  left_join(cgcs_leaid, by = c("LEAID")) %>%
  mutate(cgcsflag = if_else(!is.na(District_Name) , 1, 0)) %>%
  rename("Total_Staff" = "No Category Codes") %>%
  select(
    SCHOOL_YEAR,
    STATENAME,
    LEAID,
    LEA_NAME,
    cgcsflag,
    K12,
    Total_Enrollment,
    K12_Teachers,
    `Pre-kindergarten Teachers`,
    `Kindergarten Teachers`,
    `Elementary Teachers`,
    `Secondary Teachers`,
    `Ungraded Teachers`,
    Teachers,
    `School administrators`,
    `School Administrative Support Staff`,
    `Paraprofessionals/Instructional Aides`,
    `Librarians/media specialists`,
    `Library/Media Support Staff`,
    `Student Support Services Staff (w/o Psychology)`,
    `School Psychologists`,
    `School Staff`,
    `LEA Administrators`,
    `LEA Administrative Support Staff`,
    `Instructional Coordinators and Supervisors to the Staff`,
    `LEA Staff`,
    `Elementary School Counselors`,
    `Secondary School Counselors`,
    `School Counselors`,
    `Guidance Counselors`,
    `All Other Support Staff`,
    `Other Staff`,
    Total_Staff
  )

save.csv(Staff.All.Slim.cgcsID, paste0("District.Staff.Data_", yearx))
