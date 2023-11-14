install.packages("writexl")
library(dplyr)
library(writexl)

pae_all_counties <- read.csv("C:\\Users\\DELL\\Downloads\\PAE.csv")

process_data <- function(data) {
  data <- data %>%
    mutate(
      Labor_Force_Participation_Rate_25to29years = as.numeric(Labor_Force_Participation_Rate_25to29years),
      Labor_Force_Participation_Rate_30to34years = as.numeric(Labor_Force_Participation_Rate_30to34years),
      Labor_Force_Participation_Rate_35to44years = as.numeric(Labor_Force_Participation_Rate_35to44years),
      Labor_Force_Participation_Rate_45to54years = as.numeric(Labor_Force_Participation_Rate_45to54years),
      Unemployment_Rate_25to29years = as.numeric(Unemployment_Rate_25to29years),  
      Unemployment_Rate_30to34years = as.numeric(Unemployment_Rate_30to34years),  
      Unemployment_Rate_35to44years = as.numeric(Unemployment_Rate_35to44years),  
      Unemployment_Rate_45to54years = as.numeric(Unemployment_Rate_45to54years)   
    ) %>%
    mutate(
      Labor_Force_25to29years = (Labor_Force_Participation_Rate_25to29years * 0.01) * Total_Population_25to29years,
      Labor_Force_30to34years = (Labor_Force_Participation_Rate_30to34years * 0.01) * Total_Population_30to34years,
      Labor_Force_35to44years = (Labor_Force_Participation_Rate_35to44years * 0.01) * Total_Population_35to44years,
      Labor_Force_45to54years = (Labor_Force_Participation_Rate_45to54years * 0.01) * Total_Population_45to54years
    ) %>%
    mutate(
      Labor_Force_Total = Labor_Force_25to29years + Labor_Force_30to34years + Labor_Force_35to44years + Labor_Force_45to54years
    ) %>%
    mutate(
      Unemployed_25to29years = (Unemployment_Rate_25to29years * 0.01) * Labor_Force_25to29years,
      Unemployed_30to34years = (Unemployment_Rate_30to34years * 0.01) * Labor_Force_30to34years,
      Unemployed_35to44years = (Unemployment_Rate_35to44years * 0.01) * Labor_Force_35to44years,
      Unemployed_45to54years = (Unemployment_Rate_45to54years * 0.01) * Labor_Force_45to54years
    ) %>%
    mutate(
      Unemployed_total = Unemployed_25to29years + Unemployed_30to34years + Unemployed_35to44years + Unemployed_45to54years
    ) %>%
    mutate(
      Total_Population = Total_Population_25to29years + Total_Population_30to34years + Total_Population_35to44years + Total_Population_45to54years
    ) %>%
    mutate(
      Employed = Labor_Force_Total - Unemployed_total,
      PAE = Employed / Total_Population
    )
  
  return(data)
}


pae_NSJV <- pae_all_counties %>%
  group_by(County) %>%
  do(process_data(.)) %>%
  ungroup()

write_xlsx(pae_NSJV, "C:\\Users\\DELL\\Downloads\\NSJV_PAE.xlsx")