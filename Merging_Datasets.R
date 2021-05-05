### Prepare Data for Merging
  
##1. Load Data
library(janitor)
library(tidyverse)
library(dplyr)

# Load Stock Market Data of Pharmaceutical Companies
stock_market <- read_excel("Stock_Market_Compiled.xlsx")
# Load Daily Pharmaceutical Sales
daily_sales <- read_csv("salesdaily.csv")


## 2. Prepare Data and Merge
# Average all pharmaceutical companies' stock values by date
stock_market_by_date <-
  stock_market %>%
  clean_names() %>% 
  group_by(date) %>% 
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = funs(mean="mean")) %>% 
  mutate(date = as.Date(date))

# Reformat date to match stock market data for merging
daily_sales_date <-
  daily_sales  %>%
  clean_names() %>% 
  separate(datum, c("month", "date", "year"), "/") %>% 
  mutate(datum = paste(year, month, date, sep = "-"), 
         datum = as.Date(datum)) %>% 
  filter(!(year=="2019")) %>% # Include only 2014-2019 data
  select(c(datum, m01ab:weekday_name)) %>% 
  rename("date" = datum)

# Merge all columns of stock market data and daily sales
daily_sales_stock <- full_join(daily_sales_date, stock_market_by_date, by = c("date"))

# Write data to share with teammates
write.csv(daily_sales_stock, "daily_sales_stock.csv", row.names = F)

### 3. Prepare Merged Data
dss_renamed <-
  daily_sales_stock %>% 
  rename_at(.vars = 2:9, ~c("Med4RheumArth","Med4OstArth", "Aspirin",
                            "Ibuprofen", "Med4Tension", "Med4Sleep", 
                            "Meds4Asthma", "Meds4Allergy"))