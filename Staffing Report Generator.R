# Clear environment
#rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(scales)
library(haven)
library(stringr)
library(ggplot2)
library(kableExtra)
library(ggrepel)

#Values set yearly
set.year <- "2425"
full.year <- "2024-2025"

# Values supplied by report_template.Rmd
select.district <- params$district_name
select.state <- stringr::str_to_title(params$state_name)

#Variables for testing REMOVE once finished
#select.district <- "Los Angeles Unified School District"
#select.state <- stringr::str_to_title("CALIFORNIA")

#Custom colors
color.set <- c(
  "Other Districts" = "blue",
  "Mean" = "#e53f37",
  "Median" = "#60a862",
  setNames("#de8239", select.district),
  setNames("#f3d854", select.state)
)

#Function to read in district data
read.district.staff.fn <- function(yearx) {
  read.csv(
    paste0(
      "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/R Code/Completed Reports/",
      yearx,
      "/District.Staff.Slim_",
      yearx,
      ".csv"
    )
  )
}

#function to read in state data
read.state.staff.fn <- function(yearx) {
  read.csv(
    paste0(
      "C:/Users/bgarcia/OneDrive - Council of the Great City Schools/General/Research Requests/Staffing/R Code/Completed Reports/",
      yearx,
      "/State.Staff.Data_",
      yearx,
      ".csv"
    )
  )
}

#Create data set for districts
#calculate mean and median before identifying select district and state
#to allow the produced data set to be used across any district.
district.data.function <- function(yearx, sizex) {
  district.data <- read.district.staff.fn(yearx) %>%
    select(
      c("SCHOOL_YEAR",
        "STATENAME",
        "LEAID",
        "LEA_NAME",
        "cgcsflag",
        "Total_Enrollment",
        "Pre.kindergarten.Teachers",
        "Kindergarten.Teachers",
        "Elementary.Teachers",
        "Secondary.Teachers",
        "Ungraded.Teachers",
        "Teachers",
        "School.administrators",
        "School.Administrative.Support.Staff",
        "Paraprofessionals.Instructional.Aides",
        "Librarians.media.specialists",
        "Library.Media.Support.Staff",
        "Student.Support.Services.Staff..w.o.Psychology.",
        "School.Psychologists",
        "School.Staff",
        "LEA.Administrators",
        "LEA.Administrative.Support.Staff",
        "Instructional.Coordinators.and.Supervisors.to.the.Staff",
        "LEA.Staff",
        "Elementary.School.Counselors",
        "Secondary.School.Counselors",
        "School.Counselors",
        "Guidance.Counselors",
        "All.Other.Support.Staff",
        "Other.Staff"
      )
    )
  
  dd.set.enrollment <- district.data %>%
    filter(Total_Enrollment > sizex)
  
  dd.set.enrollment$'chart marker' <- ""
  
  #calculate mean and median. add them as separate rows with dummy var markers
  dd.mean <- dd.set.enrollment %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  
  dd.mean$SCHOOL_YEAR <- full.year
  dd.mean$STATENAME <- "Mean"
  dd.mean$LEA_NAME <- "Mean"
  dd.mean$'chart marker' <- "Mean"
  dd.mean$LEAID <- 0
  dd.mean$cgcsflag <- 0
  
  dd.median <- dd.set.enrollment %>%
    summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))
  
  dd.median$SCHOOL_YEAR <- full.year
  dd.median$STATENAME <- "Median"
  dd.median$LEA_NAME <- "Median"
  dd.median$'chart marker' <- "Median"
  dd.median$LEAID <- 0
  dd.median$cgcsflag <- 0
  
  #This file will save and be used by future functions to produce charts for designated
  #year.
  
  dd.full.alpha <- dd.set.enrollment %>%
    rbind(dd.mean) %>%
    rbind(dd.median)
  
  return(dd.full.alpha)
  
}

#pulling out state data for the selected district
state.data.function <- function(yearx) {
  state.data <- read.state.staff.fn(yearx) %>%
    rename("LEAID" = "FIPST") %>%
    rename("LEA_NAME" = "SEA_NAME") %>%
    rename("Total_Enrollment" = "STUDENT_COUNT")
  
  state.data$STATENAME <- stringr::str_to_title(state.data$STATENAME)
  
  #Calculations for the total columns that are added to state data
  state.data.teachers <- state.data %>%
    mutate(Teachers = rowSums(across(c(Pre.kindergarten.Teachers,
                                       Kindergarten.Teachers,
                                       Elementary.Teachers,
                                       Secondary.Teachers,
                                       Ungraded.Teachers)), na.rm = TRUE))
  
  state.data.t.schoolstaff <- state.data.teachers %>%
    mutate(School.Staff = rowSums(across(c(School.administrators,
                                           School.Administrative.Support.Staff,
                                           Paraprofessionals.Instructional.Aides,
                                           Librarians.media.specialists,
                                           Library.Media.Support.Staff,
                                           Student.Support.Services.Staff..w.o.Psychology.,
                                           School.Psychologists)), na.rm = TRUE))
  
  
  state.data.t.ss.administrators <- state.data.t.schoolstaff %>%
    mutate(LEA.Staff = rowSums(across(c(LEA.Administrators,
                                        LEA.Administrative.Support.Staff,
                                        Instructional.Coordinators.and.Supervisors.to.the.Staff
                                        )), na.rm = TRUE))
  
  
  state.data.t.ss.ad.counsel <- state.data.t.ss.administrators %>%
    mutate(Guidance.Counselors = rowSums(across(c(Elementary.School.Counselors,
                                                  Secondary.School.Counselors,
                                                  School.Counselors)), na.rm = TRUE))
  
  state.data.final <- state.data.t.ss.ad.counsel
  
  state.data.final$cgcsflag <- ""
  state.data.final$Other.Staff <- state.data.final$All.Other.Support.Staff
  
  return(state.data.final)
  
}

#District data and State data are complete. The following code is to specify the district and state in the data.
#dummy variable column that indicates the select district.
select.district.state.data.function <- function(yearx, districtx, statex, sizex) {

  district <- district.data.function(yearx = yearx, sizex = sizex)
  
  dd.select <- district %>%
    filter(LEA_NAME == districtx)
  
  dd.select$`chart marker` <- "District"
  
  dd.other.15k <- district %>%
    filter(LEA_NAME != districtx)
  
  state <- state.data.function(yearx)
  
  selected.state.data <- state %>%
    filter(STATENAME == statex) %>%
    mutate('chart marker' = "State")
  
  final.data.set <- dd.select %>%
    rbind(dd.other.15k) %>%
    rbind(selected.state.data)
  
  return(final.data.set)
  }

#Function for the calculation of data for staffing charts.
#District size if filtered before calculations are made
chart.data.calc.fn <- function(datax) {
  #Calculations that are required for the charts.
  data <- datax %>%
    mutate(across(c(Other.Staff, Guidance.Counselors), as.numeric))
  
  #Percent of staff that are teachers
  #Teachers/Total Staff
  total.staff <- data %>%
    mutate(Total_Staff = rowSums(across(c(Teachers,
                                          School.Staff,
                                          LEA.Staff,
                                          Guidance.Counselors,
                                          Other.Staff)),
                                 na.rm = TRUE))
  
  pct.staff.teach <- total.staff %>%
    mutate("PCTTCH" = if_else(Total_Staff > 0, Teachers / Total_Staff, NA_real_))
  
  #Pupil to Teacher Ratio
  #Total Enrollment/Teachers
  pup.tch.ratio <- pct.staff.teach %>%
    mutate("PUPTCH" = if_else(Teachers > 0, Total_Enrollment / Teachers, NA_real_))
  
  #Pupil to Total Staff Ratio
  #total enrollment/total staff
  pup.staff.ratio <- pup.tch.ratio %>%
    mutate("PUPSTF" = if_else(Total_Staff > 0, Total_Enrollment / Total_Staff, NA_real_))
  
  #Pupil to Lea Administrative Staff Ratio
  #Total Lea Administrative Staff
  #Sum Lea Admin + LEA Admin Support Staff
  
  total.lea.admin <- pup.staff.ratio %>%
    mutate("TOTLEA" = LEA.Administrators + LEA.Administrative.Support.Staff)
  
  #total enrollment/total lea administrative staff
  pup.lea.admin.ratio <- total.lea.admin %>%
    mutate("PUPLEA" = if_else(TOTLEA > 0, Total_Enrollment / TOTLEA, NA_real_))
  
  #pupil to School Administrative Staff Ratio (PUPSCH)
  #Sum School Administrators + School Administrative Staff
  total.sch.admin <- pup.lea.admin.ratio %>%
    mutate("TOTSCH" = School.administrators + School.Administrative.Support.Staff)
  
  #total enrollment/total school administrative staff
  pup.sch.admin.ratio <- total.sch.admin %>%
    mutate("PUPSCH" = if_else(TOTSCH > 0, Total_Enrollment / TOTSCH, NA_real_))
  
  #Pupil to Total Administrative Staff Ratio
  #Total Administrative Staff = TOTSCH + TOTLEA
  total.admin <- pup.sch.admin.ratio %>%
    mutate("TOTADM" = TOTLEA + TOTSCH)
  
  #Total enrollment/total administrative staff
  pup.total.admin.ratio <- total.admin %>%
    mutate("PUPADM" = if_else(TOTADM > 0, Total_Enrollment / TOTADM, NA_real_))
  
  final.data <- pup.total.admin.ratio %>%
    mutate(across(c(Total_Staff:PUPADM), as.numeric))
  
  return(final.data)
}

#Pull data together for the tables in the appendix
CGCS.appendix.table.fn <- function(yearx) {
  cgcs_table <- chart.data.calc.fn(CGCS.data.function(yearx)) %>%
    
    # Remove the Mean and Median rows
    filter(!LEA_NAME %in% c("Mean", "Median")) %>%
    
    select(
      District = LEA_NAME,
      Enrollment = Total_Enrollment,
      Total_Staff,
      Pupil_Teacher_Ratio = PUPTCH,
      Pupil_Total_Staff_Ratio = PUPSTF,
      Pupil_LEA_Admin_Ratio = PUPLEA,
      Pupil_School_Admin_Ratio = PUPSCH,
      Pupil_Total_Admin_Ratio = PUPADM
    ) %>%
    
    arrange(desc(Enrollment)) %>%
    
    mutate(
      Enrollment = scales::comma(round(Enrollment, 0)),
      Total_Staff = round(Total_Staff, 1),
      Pupil_Teacher_Ratio = round(Pupil_Teacher_Ratio, 2),
      Pupil_Total_Staff_Ratio = round(Pupil_Total_Staff_Ratio, 2),
      Pupil_LEA_Admin_Ratio = round(Pupil_LEA_Admin_Ratio, 2),
      Pupil_School_Admin_Ratio = round(Pupil_School_Admin_Ratio, 2),
      Pupil_Total_Admin_Ratio = round(Pupil_Total_Admin_Ratio, 2)
    )
  
  return(cgcs_table)
  
}
#Setting up a function for generating chart captions

build_caption <- function(chart.data,district_name,variable,percent = FALSE,measure_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(.data[[variable]])
  
  median_value <- chart.data %>%
    filter(`chart marker` == "Median") %>%
    pull(.data[[variable]])
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(.data[[variable]])
  
  fmt <- function(x) {
    if (percent) {
      scales::percent(x, accuracy = 0.1)
    } else {
      format(round(x, 2), nsmall = 2)
    }
  }
  
  stringr::str_wrap(
    glue::glue(
      "Y-axis = {measure_name}; X-axis = ranking among all school districts in sample group. Each blue dot represents a school district. {district_name} had {fmt(district_value)}. The median for the Great City Schools was {fmt(median_value)}, and the mean was {fmt(mean_value)}."
    ),
    width = 135
  )
}

#Code to dynamically create the narrative paragraphs after each chart.
#Each Chart has it's own function.
#Percent of total staff that are teachers
chart_1_narrative_fn <- function(chart.data, district_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(PUPADM)
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(PUPADM)
  
  direction <- ifelse(
    district_value > mean_value,
    "more",
    "fewer"
  )
  
  diff_pct <- abs(district_value - mean_value) / mean_value
  
  modifier <- dplyr::case_when(
    diff_pct < 0.03 ~ "slightly",
    diff_pct < 0.10 ~ "somewhat",
    TRUE ~ "substantially"
  )
  
  glue::glue(
    "{district_name} had {round(district_value, 2)} students per staff member compared to the Great City School average of {round(mean_value, 2)} students per staff member.",
    "In other words, {district_name} had {modifier} {direction} total staff than the average Great City School District.",
    "The chart above compares {district_name} and other Great City School districts with data provided by the National Center for Education Statistics;")
}

chart_2_narrative_fn <- function(chart.data, district_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(PCTTCH)
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(PCTTCH)
  
  direction <- ifelse(
    district_value > mean_value,
    "higher",
    "lower"
  )
  
  diff_pct <- abs(district_value - mean_value) / mean_value
  
  modifier <- dplyr::case_when(
    diff_pct < 0.03 ~ "slightly",
    diff_pct < 0.10 ~ "somewhat",
    TRUE ~ "substantially"
  )
  
  glue::glue(
    "{district_name} had a {modifier} {direction} percentage of staff who were teachers than the average Great City School district, ",
    "{round(district_value, 2)} vs. {round(mean_value, 2)}, respectively.",
    "The chart above compares {district_name} and other Great City School districts with data provided by the National Center for Education Statistics;")
}

chart_3_narrative_fn <- function(chart.data, district_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(PUPTCH)
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(PUPTCH)
  
  student_direction <- ifelse(
    district_value > mean_value,
    "more",
    "fewer"  )
  
  teacher_direction <- ifelse(
    district_value > mean_value,
    "fewer",
    "more"  )
  
  diff_pct <- abs(district_value - mean_value) / mean_value
  
  modifier <- dplyr::case_when(
    diff_pct < 0.03 ~ "slightly",
    diff_pct < 0.10 ~ "somewhat",
    TRUE ~ "substantially"
  )
  
  glue::glue(
    "{district_name} had {modifier} {student_direction} students per teacher than the average Great City School district, ",
    "{round(district_value, 2)} vs. {round(mean_value, 2)}, respectively. ",
    "In other words {district_name} had {modifier} {teacher_direction} teachers for its enrollment than did the average Great City School District",
    "The chart above compares {district_name} and other Great City School districts with data provided by the National Center for Education Statistics.")
}

chart_4_narrative_fn <- function(chart.data, district_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(PUPLEA)
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(PUPLEA)
  
  student_direction <- ifelse(
    district_value > mean_value,
    "more",
    "fewer"  )
  
  teacher_direction <- ifelse(
    district_value > mean_value,
    "fewer",
    "more"  )
  
  diff_pct <- abs(district_value - mean_value) / mean_value
  
  modifier <- dplyr::case_when(
    diff_pct < 0.03 ~ "slightly",
    diff_pct < 0.10 ~ "somewhat",
    TRUE ~ "substantially"
  )
  
  glue::glue(
    "{district_name} had {modifier} {student_direction} students per central office administrative staff than the average Great City School district, ",
    "{round(district_value, 2)} vs. {round(mean_value, 2)}, respectively. ",
    "In other words {district_name} had {modifier} {teacher_direction} district-level administrators and support staff for a district its size than the average Great City School District",
    "The chart above compares {district_name} and other Great City School districts with data provided by the National Center for Education Statistics;")
}

chart_5_narrative_fn <- function(chart.data, district_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(PUPSCH)
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(PUPSCH)
  
  student_direction <- ifelse(
    district_value > mean_value,
    "more",
    "fewer"  )
  
  teacher_direction <- ifelse(
    district_value > mean_value,
    "fewer",
    "more"  )
  
  diff_pct <- abs(district_value - mean_value) / mean_value
  
  modifier <- dplyr::case_when(
    diff_pct < 0.03 ~ "slightly",
    diff_pct < 0.10 ~ "somewhat",
    TRUE ~ "substantially"
  )
  
  glue::glue(
    "{district_name} had {modifier} {student_direction} students per school site administrative and support staff than the average Great City School district, ",
    "{round(district_value, 2)} vs. {round(mean_value, 2)}, respectively. ",
    "In other words {district_name} had {modifier} {teacher_direction} total school site administrators and support staff for a district its size than the average Great City School District",
    "The chart above compares {district_name} and other Great City School districts with data provided by the National Center for Education Statistics;")
}

chart_6_narrative_fn <- function(chart.data, district_name) {
  
  district_value <- chart.data %>%
    filter(`chart marker` == "District") %>%
    pull(PUPSTF)
  
  mean_value <- chart.data %>%
    filter(`chart marker` == "Mean") %>%
    pull(PUPSTF)
  
  student_direction <- ifelse(
    district_value > mean_value,
    "more",
    "fewer"  )
  
  teacher_direction <- ifelse(
    district_value > mean_value,
    "fewer",
    "more"  )
  
  diff_pct <- abs(district_value - mean_value) / mean_value
  
  modifier <- dplyr::case_when(
    diff_pct < 0.03 ~ "slightly",
    diff_pct < 0.10 ~ "somewhat",
    TRUE ~ "substantially"
  )
  
  glue::glue(
    "{district_name} had {modifier} {student_direction} students per combined school and central office staff than the average Great City School district, ",
    "{round(district_value, 2)} vs. {round(mean_value, 2)}, respectively. ",
    "In other words {district_name} had {modifier} {teacher_direction} school site and central office administrators and support staff for a district its size the average Great City School District",
    "The chart above compares {district_name} and other Great City School districts with data provided by the National Center for Education Statistics;")
}

#Charts for the report
#Global CGCS Theme for charts and legends
cgcs_theme <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    
    theme(
      # ===== Plot & Panel =====
      plot.background = element_rect(
        fill = "white",
        color = "#D9D9D9",
        linewidth = 1
      ),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(
        fill = NA,
        color = "#BFBFBF",
        linewidth = 0.8
      ),
      
      # ===== Gridlines =====
      panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "#E6E6E6", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      
      # ===== Axes =====
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      
      axis.title.x = element_text(
        size = base_size,
        face = "bold",
        margin = margin(t = 10)
      ),
      
      axis.title.y = element_text(
        size = base_size,
        face = "bold",
        margin = margin(r = 10)
      ),
      
      axis.text = element_text(size = 14, color = "#4D4D4D"),
      
      # ===== Title =====
      plot.title = element_text(
        size = base_size + 4,
        face = "bold",
        hjust = 0.5,
        color = "#1F1F1F"
      ),
      
      plot.subtitle = element_text(
        size = base_size - 2,
        hjust = 0.5,
        color = "#666666"
      ),
      
      # ===== Caption =====
      plot.caption.position = "plot",
      
      plot.caption = element_text(
        hjust = 0,
        size = 12,
        face = "italic",
        colour = "#666666"
      ),
      
      # ===== Legend =====
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      
      legend.text = element_text(size = 12, color = "#404040"),
      
      legend.background = element_blank(),
      legend.key = element_blank(),
      
      # ===== Margins =====
      plot.margin = margin(
        t = 15,
        r = 20,
        b = 15,
        l = 20
      )
    )
}

cgcs_legend <- guides(
  color = guide_legend(
    nrow = 1,
    override.aes = list(size = 5)
  )
)

#Chart Functions
Percent.of.Staff.that.are.Teachers.fn <- function(data.group,year.a,size.a,district.a,state.a) {
  
  chart.data <- chart.data.calc.fn(datax = select.district.state.data.function(
    yearx = year.a,
    districtx = district.a,
    statex = state.a,
    sizex = size.a
  ))
  
  chart.data <- chart.data %>%
    mutate(legend_group = case_when(
        `chart marker` == "District" ~ district.a,
        `chart marker` == "Mean" ~ "Mean",
        `chart marker` == "Median" ~ "Median",
        `chart marker` == "State" ~ state.a,
        TRUE ~ "Other Districts")) %>%
    mutate(plot_order = ifelse(legend_group == select.district, 1, 0)) %>%
    arrange(plot_order)
  
  chart.data.plot <- chart.data %>%
    arrange(desc(.data[[data.group]])) %>%
    mutate(rank = row_number())
  
  caption_text <- build_caption(
    chart.data = chart.data.plot,
    district_name = district.a,
    variable = "PCTTCH",
    percent = TRUE,
    measure_name = "percentage of staff who are teachers")
  
  ggplot(chart.data.plot, aes(x = rank, y = .data[[data.group]], color = legend_group)) +
    # Plot all other districts first to set them at the back
    geom_point(
      data = filter(
        chart.data.plot,
        !legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 2.8,
      alpha = 0.45
    ) +
    # Plot highlighted groups last to ensure they are visible at the front
    geom_point(
      data = filter(
        chart.data.plot,
        legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 5,
      stroke = 1.2
    ) +
    scale_color_manual(values = color.set) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(x = "District Ranking",
         y = "Percent",
         title = stringr::str_wrap(paste0("Teachers as a Percentage of Total Staff in ",select.district," and Great City School Districts"), width = 50),
         subtitle = paste0("Districts with ",size.a," students or more"),
         caption = caption_text,
         color = NULL) +
    cgcs_theme() +
    cgcs_legend +
  #Plot that overlays the data points for Mean Median, Select district and State. 
  geom_text_repel(
    data = filter(
      chart.data.plot,
      legend_group %in% c(select.district, select.state, "Mean", "Median")),
    aes(label = paste0(
      legend_group, ": ", scales::percent(.data[[data.group]], accuracy = 0.1))),
    size = 4,
    show.legend = FALSE,
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = Inf  )
}

Pupil.to.Teacher.Ratio.fn <- function(data.group,year.a,size.a,district.a,state.a) {
  
  chart.data <- chart.data.calc.fn(datax = select.district.state.data.function(
    yearx = year.a,
    districtx = district.a,
    statex = state.a,
    sizex = size.a
  ))
  
  chart.data <- chart.data %>%
    mutate(
      legend_group = case_when(
        `chart marker` == "District" ~ district.a,
        `chart marker` == "Mean" ~ "Mean",
        `chart marker` == "Median" ~ "Median",
        `chart marker` == "State" ~ state.a,
        TRUE ~ "Other Districts"
      )
    ) %>%
    mutate(plot_order = ifelse(legend_group == select.district, 1, 0)) %>%
    arrange(plot_order)

  #Removes outliers using interquartile range.  
  Q1 <- quantile(chart.data[[data.group]], 0.25, na.rm = TRUE)
  Q3 <- quantile(chart.data[[data.group]], 0.75, na.rm = TRUE)
  iqr <- IQR(chart.data[[data.group]], na.rm = TRUE)
  
  #The outlier check does not affect the district, state, mean or median
  chart.data.plot <- chart.data %>%
    filter(legend_group %in% c(select.district,
                               select.state,
                               "Mean",
                               "Median") |
        #Range set to 1.5, increase if there are legitimate districts being removed
             between(.data[[data.group]],
                Q1 - 1.5 * iqr,
                Q3 + 1.5 * iqr)) %>%
    arrange(desc(.data[[data.group]])) %>%
    mutate(rank = row_number())
  
  caption_text <- build_caption(
    chart.data = chart.data.plot,
    district_name = district.a,
    variable = data.group,
    percent = FALSE,
    measure_name = "number of students-to-teachers")
  
  # Plot all other districts first to ensure they are in the back
  ggplot(chart.data.plot, aes(x = rank, y = .data[[data.group]], color = legend_group)) +
    geom_point(
      data = filter(
        chart.data.plot,
        !legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 3,
      alpha = 0.7
    ) +
    
    # Plot highlighted groups last to ensure they are visible
    geom_point(
      data = filter(
        chart.data.plot,
        legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 5,
      alpha = 1
    ) +
    scale_color_manual(values = color.set) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n=6)) +
    labs(x = "District Ranking", 
         y = "Ratio",
         title = stringr::str_wrap(paste0("Students per Teacher in ",select.district," and Great City School Districts"), width = 50),
         subtitle = paste0("Districts with ",size.a," students or more"),
         caption = caption_text,
         color = NULL) +
    cgcs_theme() +
    cgcs_legend
}

chart.large.to.small.fn <- function(data.group,titlex,year.a,size.a,district.a,state.a,measure.a) {
  
  chart.data <- chart.data.calc.fn(
    select.district.state.data.function(
    yearx = year.a,
    sizex = size.a,
    districtx = district.a,
    statex = state.a
    )
  )
  chart.data <- chart.data %>%
    mutate(
      legend_group = case_when(
        `chart marker` == "District" ~ select.district,
        `chart marker` == "Mean" ~ "Mean",
        `chart marker` == "Median" ~ "Median",
        `chart marker` == "State" ~ select.state,
        TRUE ~ "Other Districts"
      )
    ) %>%
    mutate(plot_order = ifelse(legend_group == select.district, 1, 0)) %>%
    arrange(plot_order)
  
  #Removes outliers using interquartile range.  
  Q1 <- quantile(chart.data[[data.group]], 0.25, na.rm = TRUE)
  Q3 <- quantile(chart.data[[data.group]], 0.75, na.rm = TRUE)
  iqr <- IQR(chart.data[[data.group]], na.rm = TRUE)
  
  #The outlier check does not affect the district, state, mean or median
  chart.data.plot <- chart.data %>%
    filter(legend_group %in% c(select.district,
                               select.state,
                               "Mean",
                               "Median") |
             #Range set to 1.5, increase if there are legitimate districts being removed
             between(.data[[data.group]],
                     Q1 - 1.5 * iqr,
                     Q3 + 1.5 * iqr)) %>%
    arrange(desc(.data[[data.group]])) %>%
    mutate(rank = row_number())
  
  caption_text <- build_caption(
    chart.data = chart.data.plot,
    district_name = district.a,
    variable = data.group,
    percent = FALSE,
    measure_name = measure.a)
  
  
  ggplot(chart.data.plot, aes(x = rank, y = .data[[data.group]], color = legend_group)) +
    # Plot all other districts first
    geom_point(
      data = filter(
        chart.data.plot,!legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
     size = 2.8,
      alpha = 0.45
    ) +
    # Plot highlighted groups last
    geom_point(
      data = filter(
        chart.data.plot,
        legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 5,
      stroke = 1.2
    ) +
    scale_color_manual(values = color.set) +
    scale_x_continuous(breaks = seq(0, max(chart.data.plot$rank), by = 50)) +
    scale_y_continuous() +
    labs(x = "District Ranking",
         y = "Ratio", 
         title = stringr::str_wrap(titlex, width = 50),
         subtitle = paste0("Districts with ",size.a," students or more"),
         caption = caption_text,
         color = NULL) +
    cgcs_theme() +
    cgcs_legend
}

#Chart 1 Student to Total District Staff Ratio in DISTRICTX and Great City School Districts
fig.1 <- chart.large.to.small.fn(  "PUPADM",
                                   paste0("Student to Total District Staff Ratio in ",select.district," and Great City School Districts"),
                                   set.year,
                                   15000,
                                   select.district,
                                   select.state,
                                   "number of students-to-total staff")
#Chart 2 Teachers as a Percentage of Total Staff in DISTRICTX and Great City School Districts
fig.2 <- Percent.of.Staff.that.are.Teachers.fn("PCTTCH", 
                                               set.year, 
                                               15000, 
                                               select.district, 
                                               select.state)
#Chart 3 Students per Teacher in DISTRICTX and Great City School Districts
fig.3 <- Pupil.to.Teacher.Ratio.fn("PUPTCH", 
                                   set.year, 
                                   15000, 
                                   select.district, 
                                   select.state)
#Chart 4 Students per Total Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.4 <- chart.large.to.small.fn("PUPLEA",
                                 paste0("Students per Total Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 15000,
                                 select.district,
                                 select.state,
                                 "number of students per central office administrative and support staff.")
#Chart 5 Students per Total School Site Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.5 <- chart.large.to.small.fn("PUPSCH",
                                 paste0("Students per Total School Site Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 15000,
                                 select.district,
                                 select.state,
                                 "number of students per administrator")
#Chart 6 Students per Combined School and Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.6 <- chart.large.to.small.fn("PUPSTF",
                                 paste0("Students per Combined School and Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 15000,
                                 select.district,
                                 select.state,
                                 "number of students per combined school-based and central office administrators and support staff")

#Districts with Enrollments of 50K or greater
#Chart 1 Student to Total District Staff Ratio in DISTRICTX and Great City School Districts
fig.7 <- chart.large.to.small.fn(  "PUPADM",
                                   paste0("Student to Total District Staff Ratio in ",select.district," and Great City School Districts"),
                                   set.year,
                                   50000,
                                   select.district,
                                   select.state,
                                   "number of students-to-total staff")
#Chart 2 Teachers as a Percentage of Total Staff in DISTRICTX and Great City School Districts
fig.8 <- Percent.of.Staff.that.are.Teachers.fn("PCTTCH", 
                                               set.year, 
                                               50000, 
                                               select.district, 
                                               select.state)
#Chart 3 Students per Teacher in DISTRICTX and Great City School Districts
fig.9 <- Pupil.to.Teacher.Ratio.fn("PUPTCH", 
                                   set.year, 
                                   50000, 
                                   select.district, 
                                   select.state)
#Chart 4 Students per Total Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.10 <- chart.large.to.small.fn("PUPLEA",
                                 paste0("Students per Total Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 50000,
                                 select.district,
                                 select.state,
                                 "number of students per central office administrative and support staff.")
#Chart 5 Students per Total School Site Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.11 <- chart.large.to.small.fn("PUPSCH",
                                 paste0("Students per Total School Site Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 50000,
                                 select.district,
                                 select.state,
                                 "number of students per administrator")
#Chart 6 Students per Combined School and Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.12 <- chart.large.to.small.fn("PUPSTF",
                                 paste0("Students per Combined School and Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 50000,
                                 select.district,
                                 select.state,
                                 "number of students per combined school-based and central office administrators and support staff")

#Districts with enrollments over 100k
#Chart 1 Student to Total District Staff Ratio in DISTRICTX and Great City School Districts
fig.13 <- chart.large.to.small.fn(  "PUPADM",
                                   paste0("Student to Total District Staff Ratio in ",select.district," and Great City School Districts"),
                                   set.year,
                                   100000,
                                   select.district,
                                   select.state,
                                   "number of students-to-total staff")
#Chart 2 Teachers as a Percentage of Total Staff in DISTRICTX and Great City School Districts
fig.14 <- Percent.of.Staff.that.are.Teachers.fn("PCTTCH", 
                                               set.year, 
                                               100000, 
                                               select.district, 
                                               select.state)
#Chart 3 Students per Teacher in DISTRICTX and Great City School Districts
fig.15 <- Pupil.to.Teacher.Ratio.fn("PUPTCH", 
                                   set.year, 
                                   100000, 
                                   select.district, 
                                   select.state)
#Chart 4 Students per Total Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.16 <- chart.large.to.small.fn("PUPLEA",
                                 paste0("Students per Total Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 100000,
                                 select.district,
                                 select.state,
                                 "number of students per central office administrative and support staff.")
#Chart 5 Students per Total School Site Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.17 <- chart.large.to.small.fn("PUPSCH",
                                 paste0("Students per Total School Site Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 100000,
                                 select.district,
                                 select.state,
                                 "number of students per administrator")
#Chart 6 Students per Combined School and Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.18 <- chart.large.to.small.fn("PUPSTF",
                                 paste0("Students per Combined School and Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 100000,
                                 select.district,
                                 select.state,
                                 "number of students per combined school-based and central office administrators and support staff")

#CGCS Districts only
#A number of the functions have to be modified to filter for just Council
#District filtering sill takes place in district.data.filter renamed to cgcs.data.function
#Function filters for cgcs only districts then calculates mean & median
CGCS.data.function <- function(yearx) {
  district.data <- read.district.staff.fn(yearx) %>%
    select(
      c("SCHOOL_YEAR",
        "STATENAME",
        "LEAID",
        "LEA_NAME",
        "cgcsflag",
        "Total_Enrollment",
        "Pre.kindergarten.Teachers",
        "Kindergarten.Teachers",
        "Elementary.Teachers",
        "Secondary.Teachers",
        "Ungraded.Teachers",
        "Teachers",
        "School.administrators",
        "School.Administrative.Support.Staff",
        "Paraprofessionals.Instructional.Aides",
        "Librarians.media.specialists",
        "Library.Media.Support.Staff",
        "Student.Support.Services.Staff..w.o.Psychology.",
        "School.Psychologists",
        "School.Staff",
        "LEA.Administrators",
        "LEA.Administrative.Support.Staff",
        "Instructional.Coordinators.and.Supervisors.to.the.Staff",
        "LEA.Staff",
        "Elementary.School.Counselors",
        "Secondary.School.Counselors",
        "School.Counselors",
        "Guidance.Counselors",
        "All.Other.Support.Staff",
        "Other.Staff"))
  
  dd.set.enrollment <- district.data %>%
    filter(cgcsflag == 1)
  
  dd.set.enrollment$'chart marker' <- ""
  
  #calculate mean and median. add them as separate rows with dummy var markers
  dd.mean <- dd.set.enrollment %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  
  dd.mean$SCHOOL_YEAR <- full.year
  dd.mean$STATENAME <- "Mean"
  dd.mean$LEA_NAME <- "Mean"
  dd.mean$'chart marker' <- "Mean"
  dd.mean$LEAID <- 0
  dd.mean$cgcsflag <- 0
  
  dd.median <- dd.set.enrollment %>%
    summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))
  
  dd.median$SCHOOL_YEAR <- full.year
  dd.median$STATENAME <- "Median"
  dd.median$LEA_NAME <- "Median"
  dd.median$'chart marker' <- "Median"
  dd.median$LEAID <- 0
  dd.median$cgcsflag <- 0
  
  #This file will save and be used by future functions to produce charts for designated
  #year.
  
  dd.full.alpha <- dd.set.enrollment %>%
    rbind(dd.mean) %>%
    rbind(dd.median)
  
  return(dd.full.alpha)
  
}

#District data and State data are complete. The following code is to specify the district and state in the data.
#dummy variable column that indicates the select district.
select.cgcs.state.data.function <- function(yearx, districtx, statex) {
  district <- CGCS.data.function(yearx = yearx)
  
  dd.select <- district %>%
    filter(LEA_NAME == districtx)
  
  dd.select$`chart marker` <- "District"
  
  dd.other.15k <- district %>%
    filter(LEA_NAME != districtx)
  
  state <- state.data.function(yearx)
  
  selected.state.data <- state %>%
    filter(STATENAME == statex) %>%
    mutate('chart marker' = "State")
  
  final.data.set <- dd.select %>%
    rbind(dd.other.15k) %>%
    rbind(selected.state.data)
  
  return(final.data.set)
  
}

#Charts for the report
#Chart Functions
CGCS.Percent.of.Staff.that.are.Teachers.fn <- function(data.group,year.a,district.a,state.a) {
  
  chart.data <- chart.data.calc.fn(datax = select.cgcs.state.data.function(
    yearx = year.a,
    districtx = district.a,
    statex = state.a))
  
  chart.data <- chart.data %>%
    mutate(legend_group = case_when(
      `chart marker` == "District" ~ district.a,
      `chart marker` == "Mean" ~ "Mean",
      `chart marker` == "Median" ~ "Median",
      `chart marker` == "State" ~ state.a,
      TRUE ~ "Other Districts")) %>%
    mutate(plot_order = ifelse(legend_group == select.district, 1, 0)) %>%
    arrange(plot_order)
  
  chart.data.plot <- chart.data %>%
    arrange(desc(.data[[data.group]])) %>%
    mutate(rank = row_number())
  
  caption_text <- build_caption(
    chart.data = chart.data.plot,
    district_name = district.a,
    variable = "PCTTCH",
    percent = TRUE,
    measure_name = "percentage of staff who are teachers")
  
  ggplot(chart.data.plot, aes(x = rank, y = .data[[data.group]], color = legend_group)) +
    # Plot all other districts first to set them at the back
    geom_point(
      data = filter(
        chart.data.plot,
        !legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 2.8,
      alpha = 0.45
    ) +
    # Plot highlighted groups last to ensure they are visible at the front
    geom_point(
      data = filter(
        chart.data.plot,
        legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 5,
      stroke = 1.2
    ) +
    scale_color_manual(values = color.set) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(x = "District Ranking",
         y = "Percent",
         title = stringr::str_wrap(paste0("Teachers as a Percentage of Total Staff in ",select.district," and Great City School Districts"), width = 50),
         subtitle = paste0("All CGCS Districts"),
         caption = caption_text,
         color = NULL) +
    cgcs_theme() +
    cgcs_legend
}

CGCS.Pupil.to.Teacher.Ratio.fn <- function(data.group,year.a,district.a,state.a) {
  
  chart.data <- chart.data.calc.fn(datax = select.cgcs.state.data.function(
    yearx = year.a,
    districtx = district.a,
    statex = state.a))
  
  chart.data <- chart.data %>%
    mutate(
      legend_group = case_when(
        `chart marker` == "District" ~ district.a,
        `chart marker` == "Mean" ~ "Mean",
        `chart marker` == "Median" ~ "Median",
        `chart marker` == "State" ~ state.a,
        TRUE ~ "Other Districts"
      )
    ) %>%
    mutate(plot_order = ifelse(legend_group == select.district, 1, 0)) %>%
    arrange(plot_order)
  
  #Removes outliers using interquartile range.  
  Q1 <- quantile(chart.data[[data.group]], 0.25, na.rm = TRUE)
  Q3 <- quantile(chart.data[[data.group]], 0.75, na.rm = TRUE)
  iqr <- IQR(chart.data[[data.group]], na.rm = TRUE)
  
  #The outlier check does not affect the district, state, mean or median
  chart.data.plot <- chart.data %>%
    filter(legend_group %in% c(select.district,
                               select.state,
                               "Mean",
                               "Median") |
             #Range set to 1.5, increase if there are legitimate districts being removed
             between(.data[[data.group]],
                     Q1 - 1.5 * iqr,
                     Q3 + 1.5 * iqr)) %>%
    arrange(desc(.data[[data.group]])) %>%
    mutate(rank = row_number())
  
  caption_text <- build_caption(
    chart.data = chart.data.plot,
    district_name = district.a,
    variable = data.group,
    percent = FALSE,
    measure_name = "number of students-to-teachers")
  
  # Plot all other districts first to ensure they are in the back
  ggplot(chart.data.plot, aes(x = rank, y = .data[[data.group]], color = legend_group)) +
    geom_point(
      data = filter(
        chart.data.plot,
        !legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 3,
      alpha = 0.7
    ) +
    
    # Plot highlighted groups last to ensure they are visible
    geom_point(
      data = filter(
        chart.data.plot,
        legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 5,
      alpha = 1
    ) +
    scale_color_manual(values = color.set) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n=6)) +
    labs(x = "District Ranking", 
         y = "Ratio",
         title = stringr::str_wrap(paste0("Students per Teacher in ",select.district," and Great City School Districts"), width = 50),
         subtitle = "All CGCS Districts",
         caption = caption_text,
         color = NULL) +
    cgcs_theme() +
    cgcs_legend
}

CGCS.chart.large.to.small.fn <- function(data.group,titlex,year.a,district.a,state.a,measure.a) {
  
  chart.data <- chart.data.calc.fn(
    select.cgcs.state.data.function(
    yearx = year.a,
    districtx = district.a,
    statex = state.a))
  
  chart.data <- chart.data %>%
    mutate(
      legend_group = case_when(
        `chart marker` == "District" ~ select.district,
        `chart marker` == "Mean" ~ "Mean",
        `chart marker` == "Median" ~ "Median",
        `chart marker` == "State" ~ select.state,
        TRUE ~ "Other Districts"
      )
    ) %>%
    mutate(plot_order = ifelse(legend_group == select.district, 1, 0)) %>%
    arrange(plot_order)
  
  #Removes outliers using interquartile range.  
  Q1 <- quantile(chart.data[[data.group]], 0.25, na.rm = TRUE)
  Q3 <- quantile(chart.data[[data.group]], 0.75, na.rm = TRUE)
  iqr <- IQR(chart.data[[data.group]], na.rm = TRUE)
  
  #The outlier check does not affect the district, state, mean or median
  chart.data.plot <- chart.data %>%
    filter(legend_group %in% c(select.district,
                               select.state,
                               "Mean",
                               "Median") |
             #Range set to 1.5, increase if there are legitimate districts being removed
             between(.data[[data.group]],
                     Q1 - 1.5 * iqr,
                     Q3 + 1.5 * iqr)) %>%
    arrange(desc(.data[[data.group]])) %>%
    mutate(rank = row_number())
  
  caption_text <- build_caption(
    chart.data = chart.data.plot,
    district_name = district.a,
    variable = data.group,
    percent = FALSE,
    measure_name = measure.a)
  
  
  ggplot(chart.data.plot, aes(x = rank, y = .data[[data.group]], color = legend_group)) +
    # Plot all other districts first
    geom_point(
      data = filter(
        chart.data.plot,!legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 2.8,
      alpha = 0.45
    ) +
    # Plot highlighted groups last
    geom_point(
      data = filter(
        chart.data.plot,
        legend_group %in% c(select.district, select.state, "Mean", "Median")
      ),
      size = 5,
      stroke = 1.2
    ) +
    scale_color_manual(values = color.set) +
    scale_x_continuous(breaks = seq(0, max(chart.data.plot$rank), by = 50)) +
    scale_y_continuous() +
    labs(x = "District Ranking",
         y = "Ratio", 
         title = stringr::str_wrap(titlex, width = 50),
         subtitle = "All CGCS Districts",
         caption = caption_text,
         color = NULL) +
    cgcs_theme() +
    cgcs_legend
}

#Chart 1 Student to Total District Staff Ratio in DISTRICTX and Great City School Districts
fig.19 <- CGCS.chart.large.to.small.fn(  "PUPADM",
                                   paste0("Student to Total District Staff Ratio in ",select.district," and Great City School Districts"),
                                   set.year,
                                   select.district,
                                   select.state,
                                   "number of students-to-total staff")
#Chart 2 Teachers as a Percentage of Total Staff in DISTRICTX and Great City School Districts
fig.20 <- CGCS.Percent.of.Staff.that.are.Teachers.fn("PCTTCH", 
                                               set.year, 
                                               select.district, 
                                               select.state)
#Chart 3 Students per Teacher in DISTRICTX and Great City School Districts
fig.21 <- CGCS.Pupil.to.Teacher.Ratio.fn("PUPTCH", 
                                   set.year, 
                                   select.district, 
                                   select.state)
#Chart 4 Students per Total Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.22 <- CGCS.chart.large.to.small.fn("PUPLEA",
                                 paste0("Students per Total Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 select.district,
                                 select.state,
                                 "number of students per central office administrative and support staff.")
#Chart 5 Students per Total School Site Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.23 <- CGCS.chart.large.to.small.fn("PUPSCH",
                                 paste0("Students per Total School Site Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 select.district,
                                 select.state,
                                 "number of students per administrator")
#Chart 6 Students per Combined School and Central Office Administrative and Support Staff in DISTRICTX and Great City School Districts
fig.24 <- CGCS.chart.large.to.small.fn("PUPSTF",
                                 paste0("Students per Combined School and Central Office Administrative and Support Staff in ",select.district," and Great City School Districts"),
                                 set.year,
                                 select.district,
                                 select.state,
                                 "number of students per combined school-based and central office administrators and support staff")
#Appendix creation
appendix_cgcs_table <- CGCS.appendix.table.fn(set.year)
