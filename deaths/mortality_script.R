library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(readr)
library(janitor)

#################################################

# Covid deaths data from WHO

wdata <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
colnames(wdata)[1] <- "date"
wdata$week <- format(as.Date(wdata$date), format="%Y-%U")
wdata

#imp_data <- filter(wdata, Country=="Austria" | Country=="Belgium" | Country=="Denmark" | Country=="France" | Country=="Germany" | Country=="Italy" | Country=="Poland" | Country=="Portugal" | Country=="Spain" | Country=="Sweden" | Country=="Switzerland" | Country=="United States of America")
imp_data <- wdata
imp_data$week <- format(as.Date(imp_data$date), format="%Y-%U")
imp_data <- ddply(imp_data, .(Country, week), summarize, deaths=sum(New_deaths))
imp_data <- imp_data %>% mutate(year = substr(week, 1, 4), week = as.integer(substr(week, 6,7)), deaths_cumsum = cumsum(deaths))
colnames(imp_data)[1] <- "country"
imp_data[imp_data$country == "United States of America",]$country <- "United_States"

write.csv(imp_data, "data_covid.csv")

#################################################

# Overall deaths data from mortality.org

stmf <- read_csv("https://www.mortality.org/Public/STMF/Outputs/stmf.csv", skip=1)

deaths <- stmf %>%
  clean_names() %>%
  select(country_code:d_total) %>%
  pivot_longer(5:10,
               names_to = "age", values_to = "deaths",
               names_pattern = "[d_]*([a-z0-9_p]*)"
  ) %>%
  filter(age == "total", sex == "b") %>%
  mutate(
    country = recode(country_code,
                     AUT = "Austria",
                     BEL = "Belgium",
                     BGR = "Bulgaria",
                     CHE = "Switzerland",
                     CZE = "Czechia",
                     DNK = "Denmark",
                     ESP = "Spain",
                     EST = "Estonia",
                     FRATNP = "France",
                     DEUTNP = "Germany",
                     FIN = "Finland",
                     GBRTENW = "United_Kingdom",
                     GRC = "Greece",
                     HRV = "Croatia",
                     HUN = "Hungary",
                     ISL = "Iceland",
                     ISR = "Israel",
                     ITA = "Italy",
                     LTU = "Lithuania",
                     LUX = "Luxembourg",
                     LVA = "Latvia",
                     NLD = "Netherlands",
                     NOR = "Norway",
                     POL = "Poland",
                     PRT = "Portugal",
                     RUS = "Russia",
                     SVK = "Slovakia",
                     SVN = "Slovenia",
                     SWE = "Sweden",
                     USA = "United_States")
  ) %>% select(year, week, country, deaths)

deaths <- filter(deaths, country != "GBR_SCO" & country != "GBR_NIR")
deaths <- mutate(deaths, is2020 = ifelse(year==2020, TRUE, FALSE))
readr::write_csv(deaths, path = "data_mortality.csv")
