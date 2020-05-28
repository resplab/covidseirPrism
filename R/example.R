library(prism)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(patchwork)
library(gghighlight)


url <- "https://docs.google.com/spreadsheets/d/1ad7-09_Jn6AxsdkVPE33T-iLfGpPRmd3piXQqFiVeas/export?&format=csv"

CanadaCases <- read_csv(url)
CanadaCases <- read_csv(url)

covidCases <-  CanadaCases %>% rename (name = "prname")  %>% rename (Cases = "numconf")  %>% mutate(date=dmy(date)) %>%
  mutate(name = replace(name, name == "British Columbia", "BC")) %>%
  mutate(name = replace(name, name == "Ontario", "ON")) %>%
  mutate(name = replace(name, name == "Alberta", "AB")) %>%
  mutate(name = replace(name, name == "Saskatchewan", "SK")) %>%
  mutate(name = replace(name, name == "Manitoba", "MB")) %>%
  mutate(name = replace(name, name == "Quebec", "QC")) %>%
  mutate(name = replace(name, name == "Nova Scotia", "NS")) %>%
  mutate(name = replace(name, name == "New Brunswick", "NB")) %>%
  mutate(name = replace(name, name == "Newfoundland and Labrador", "NL"))%>%
  mutate(name = replace(name, name == "Prince Edward Island", "PE")) %>%
  mutate(name = replace(name, name == "Yukon", "YT")) %>%
  mutate(name = replace(name, name == "Northwest Territories", "NT")) %>%
  mutate(name = replace(name, name == "Nunavut", "NU")) %>%
  filter (name!="Canada") %>% filter (date!=today())

bcCases <- covidCases %>% filter (name == "BC")

#connect_to_model("covidseirPrism", api_key = "123456", address = "localhost:5656")
connect_to_model("covidseirPrism", api_key = "123456", address = "covidseir.cp.prism-ubc.linaralabs.com")

input <- get_default_input()
first <- length(bcCases$numtoday)-42+1
last <- length(bcCases$numtoday)
input$daily_cases <- bcCases$numtoday[first:last]

# Example assumed sampling fractions of positive cases:
s1 <- c(rep(0.1, 13), rep(0.2, length(input$daily_cases) - 13))

samp_frac_seg <- c(rep(1, 13), rep(2, length(input$daily_cases) - 13))
s2 <- rep(0.07, length(input$daily_cases)) # Assuming 7\% of positive individuals are hospitalized

input$samp_frac_fixed <- rep(0.1, length(input$daily_cases))


input$fit_iter <- 100
input$chains <- 1

input$f_fixed <- rep(0.1, 90)
#input$forecast_iter <- 1:25
results <- model_run(input)
draw_plots()


