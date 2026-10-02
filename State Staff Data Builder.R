#Clear environment
rm(list = ls())

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

#Functions
#functions for reading in the different files. for EL & SWD the files are csv for 2021 and after
read.csv.staff <- function(yearx) {
  read.csv(
    paste0(
      "C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Staff Data/",
      yearx,
      "_state.csv"
    )
  )
}

#Function to read in the student enrollment CCD file
read.csv.grs <- function(yearx) {
  read.csv(
    paste0(
      "C:/Users/Bgarcia/OneDrive - Council of the Great City Schools/Desktop/R Scripts/Database Files/Grade.Race.Sex/",
      yearx,
      "_state.csv"
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

##test for the data file
df.grs.raw <- read.csv.grs(yearx)

names(df.grs.raw)
summary(df.grs.raw$STUDENT_COUNT)

#need to filter total-indicator for just Subtotal 4 to get a K12 and PK12 total along with ED Unit Total to get the total enrollment.
#Also filter for dms flag != derived
df.grs.2 <- df.grs.raw %>%
  filter(TOTAL_INDICATOR == "Education Unit Total") %>%
  filter(DMS_FLAG == "Reported") %>%
  subset(
    select = c(
      "SCHOOL_YEAR",
      "FIPST",
      "STATENAME",
      "ST",
      "SEA_NAME",
      "STATE_AGENCY_NO",
      "STUDENT_COUNT",
      "TOTAL_INDICATOR" ,
      "DMS_FLAG"
    )
  )

df.grs.3 <- df.grs.raw %>%
  filter(TOTAL_INDICATOR == "Education Unit Total") %>%
  filter(DMS_FLAG == "Reported") %>%
  subset(select = c("STATENAME", "STUDENT_COUNT"))

#staff data
#pulls in CCD staff file for identified year.
staff.raw <- read.csv.staff(yearx)

names(staff.raw)

staff.wide <- staff.raw %>%
  filter(DMS_FLAG == "Reported") %>%
  filter(TOTAL_INDICATOR != "Education Unit Total") %>%
  pivot_wider(names_from = "STAFF", values_from = "STAFF_COUNT")

State.staff.enrollment <- df.grs.3 %>%
  left_join(staff.wide, by = c("STATENAME")) %>%
  select(
    SCHOOL_YEAR,
    STATENAME,
    FIPST,
    SEA_NAME,
    STUDENT_COUNT,
    `Pre-kindergarten Teachers`,
    `Kindergarten Teachers`,
    `Elementary Teachers`,
    `Secondary Teachers`,
    `Ungraded Teachers`,
    `School administrators`,
    `School Administrative Support Staff`,
    `Paraprofessionals/Instructional Aides`,
    `Librarians/media specialists`,
    `Library/Media Support Staff`,
    `Student Support Services Staff (w/o Psychology)`,
    `School Psychologists`,
    `LEA Administrators`,
    `LEA Administrative Support Staff`,
    `Instructional Coordinators and Supervisors to the Staff`,
    `Elementary School Counselors`,
    `Secondary School Counselors`,
    `School Counselors`,
    `All Other Support Staff`
  )


save.csv(State.staff.enrollment, paste0("State.Staff.Data_", yearx))