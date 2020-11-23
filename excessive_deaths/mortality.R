library(eurostat)
library(ggplot2)

mdata <- eurostat::get_eurostat("demo_r_mwk_10")

mdata2010 <- mdata[as.character(mdata$time) >= "2010",]

age_group <- c("Y_LT10" = "<10", "Y10-19" = "10-19", "Y20-29" = "20-29", 
               "Y30-39" = "30-39", "Y40-49" = "40-49", "Y50-59" = "50-59", 
               "Y60-69" = "60-69", "Y70-79" = "70-79", "Y_GE80" = "> 80")
mdata2010 <- mdata2010[mdata2010$age %in% names(age_group), ]
mdata2010$age <- factor(mdata2010$age, levels = names(age_group), labels = age_group)

geo_group <- c("SE" = "Sweden", "BE" = "Belgium", "ES" = "Spain", 
               "UK" = "United Kingdom", "FR" = "France", "PL" = "Poland", 
               "DE" = "Germany", "IT" = "Italy")

#geo_group <- c("PL" = "Poland")

mdata2010 <- mdata2010[mdata2010$geo %in% names(geo_group), ]
mdata2010$geo <- factor(mdata2010$geo, levels = names(geo_group), labels = geo_group)

#mdata2010 <- mdata2010[mdata2010$sex %in% c("F","M"), ]
mdata2010 <- mdata2010[mdata2010$sex %in% c("T"), ]

mdata2010$year <- substr(mdata2010$time, 1, 4)
mdata2010$week <- as.numeric(substr(mdata2010$time, 6, 7))



ggplot(mdata2010, aes(week, values, group=paste(sex, year))) +
  geom_line(data = mdata2010[mdata2010$year!="2020",], alpha = 0.1) + 
  geom_smooth(data = mdata2010[mdata2010$year!="2020",], se = FALSE, group = 1, color = "black", size=0.6) +
  geom_hline(yintercept = 0, color="grey", size=0.5) + 
  geom_step(data = mdata2010[mdata2010$year=="2020",], color = "red3") +
  facet_wrap(geo~age, scales = "free_y", ncol = 9) + xlim(0,52) + ylab("Number of deaths (eurostat)") +
  geom_vline(xintercept = seq(0,50,10), color="grey", lty=3)+
  DALEX::theme_ema() + ggtitle("Excessive deaths in 2020 by age\nRed - data for 2020, grey - data for 2010-2019, black - average for 2010-2019") +
  theme(axis.text = element_text(size=5),
        axis.text.x = element_text(size=5),
        axis.text.y = element_text(size=5), 
        strip.text = element_text(size=5))
