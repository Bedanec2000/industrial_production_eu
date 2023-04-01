# SETUP

install.packages("tidyverse")
library("tidyverse")
install.packages("scales")
library("scales")
install.packages("lubridate")
library("lubridate")
library(readr)
sts_inpr_m_custom_5572544_page_linear <- read_csv("sts_inpr_m__custom_5572544_page_linear.csv", 
                                                  col_types = cols(TIME_PERIOD = col_date(format = "%Y-%m")))

df <- sts_inpr_m_custom_5572544_page_linear

# Cleaning

sub <- df %>% select('geo','TIME_PERIOD', "OBS_VALUE")
sub_all <- filter(sub, geo == "BE" | geo == "BG" | geo == "DK" | geo == "DE"
                 | geo == "EE" | geo == "FI" | geo == "FR" | geo == "EL" | geo == "IE"
                 | geo == "IT" | geo == "HR" | geo == "LV" | geo == "LT"
                 | geo == "LU" | geo == "MT" | geo == "NL" | geo == "AT"
                 | geo == "PL" | geo == "PT" | geo == "RO" | geo == "SE"
                 | geo == "SK" | geo == "SI" | geo == "ES" | geo == "CZ"
                 | geo == "HU" | geo == "CY" | geo =="UK")


# Graph 3-Ländervergleich

sub_all %>% 
  filter(geo=="AT" |geo== "DE" | geo=="SI") %>% 
  ggplot(., aes(x=TIME_PERIOD, y=OBS_VALUE, colour=geo))+
  geom_line()+
  labs(title = "Industrieproduktionsindex nach Land",
       x="Jahr", y="Produktionsindex")


#EU alle Länder

sub_all %>% 
  ggplot(., aes(x=TIME_PERIOD, y=OBS_VALUE, color=geo))+
  geom_line()+
  geom_vline(xintercept = as.numeric(as.Date("2015-01-01")),
             color="red", lwd=0.3, linetype="dashed")+
  theme(legend.text = element_text(colour="gray", size=10, face="bold"))+
  labs(color = "Staat:")+
  
  labs(title = "Industrieproduktionsindex nach Land", subtitle = "Datenquelle: EUROSTAT | 2015 = 100",
       x="Jahr (monatliche Updates)", y="Produktionsindex")+

  annotate(geom = "rect", xmin = ymd('2008-01-01'), xmax = ymd('2009-12-01'), ymin = -Inf, ymax = Inf,
           fill = "palegreen", colour = NA, alpha = 0.2) +
  annotate(geom = "rect", xmin = ymd('2019-11-01'), xmax = ymd('2021-12-01'), ymin = -Inf, ymax = Inf,
           fill = "red", colour = NA, alpha = 0.2) +
  
  facet_wrap(~ geo)

