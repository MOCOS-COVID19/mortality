library(ggplot2)
library(DALEX)

data_mortality <- read.csv("data_mortality.csv")
data_covid <- read.csv("data_covid.csv")


ggplot(data_mortality, aes(x = week, y = deaths, group = year)) + 
  geom_line(aes(col = is2020)) +
  geom_line(data = data_covid, aes(x = week, y = deaths)) +
  facet_wrap(~ country, scales = 'free_y') +
  scale_color_manual(values = c("FALSE" = 'gray', "TRUE" = 'red')) +
  guides(col = FALSE) 


country <- c("Austria", "Belgium", "Denmark", "France", "Germany", "Italy", "Poland", "Portugal", "Spain", "Sweden", "Switzerland", "United_States")

data_mortality_2020 <- data_mortality[data_mortality$is2020,]

deaths_2020 <- merge(data_covid, data_mortality_2020, by = c("year","week","country"))
deaths_2020$deaths.x <- pmax(0, deaths_2020$deaths.x)
deaths_2020$net <- deaths_2020$deaths.y - deaths_2020$deaths.x

ggplot(data_mortality[data_mortality$country %in% country,], aes(x = week, y = deaths, group = year)) + 
  geom_ribbon(data = deaths_2020[deaths_2020$country %in% country,], aes(x = week, ymin = net, ymax = deaths.y, y = deaths.y), alpha=0.3, fill = "red") +
  geom_line(aes(col = is2020), alpha=0.5) +
  geom_line(data = deaths_2020[deaths_2020$country %in% country,], aes(y = deaths.y, col = is2020)) +
  facet_wrap(~ country, scales = 'free_y', ncol = 3) +
#  facet_wrap(~ country, ncol = 3) +
  scale_color_manual(values = c("FALSE" = 'gray', "TRUE" = 'red')) +
  guides(col = FALSE)  + DALEX::theme_ema() + ylim(0,NA) +
  ggtitle("Number of deaths in 2000-2020. Red - data for 2020. Ribbon - covid deaths")




ggplot(data_mortality[data_mortality$country %in% country,], aes(x = week, y = deaths, group = year)) + 
  geom_ribbon(data = deaths_2020[deaths_2020$country %in% country,], aes(x = week, ymin = net, ymax = deaths.y, y = deaths.y), alpha=0.3, fill = "red") +
  geom_line(aes(col = is2020), alpha=0.5) +
  geom_line(data = deaths_2020[deaths_2020$country %in% country,], aes(y = deaths.y, col = is2020)) +
  facet_wrap(~ country, scales = 'free_y', ncol = 3) +
  #  facet_wrap(~ country, ncol = 3) +
  scale_color_manual(values = c("FALSE" = 'gray', "TRUE" = 'red')) +
  guides(col = FALSE)  + DALEX::theme_ema() + xlab("Tydzień w roku") + ylab("Liczba zgonów") +
  ggtitle("Liczba zgonów w latach 2000-2020. Czerwona linia to zgony w roku 2020, czerwone pola to zgony raportowane jako COVID-19.\n")

