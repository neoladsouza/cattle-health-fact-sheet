rm(list=ls())
library(tidyverse)
library(gridExtra)

# LOAD IN DATA
cattleData <- readRDS("G:/Shared drives/AREC280F23/Data/USDA-NASS Quick Stats/Livestock/Cattle.RDS")

# filter data for OPTION 1: cattle condition
cattleCondition <- cattleData %>%
  filter(SECTOR_DESC == "ANIMALS & PRODUCTS", GROUP_DESC == "LIVESTOCK", 
         COMMODITY_DESC == "CATTLE", STATISTICCAT_DESC == "CONDITION", 
         CLASS_DESC == "INCL CALVES")

# select variables for OPTION 1
cattleConditionSelected <- cattleCondition %>%
  select(YEAR, REFERENCE_PERIOD_DESC, STATE_NAME, UNIT_DESC, VALUE) %>%
  arrange(STATE_NAME, YEAR, REFERENCE_PERIOD_DESC)

# pivot data to get data per week
cattlePivoted <- cattleConditionSelected %>%
  group_by(STATE_NAME,REFERENCE_PERIOD_DESC)%>%
  pivot_wider(names_from = UNIT_DESC, values_from = VALUE)

cattleNumeric <- cattlePivoted %>%
  # create new columns with numeric versions of data
  mutate(pct_poor = as.numeric(`PCT POOR`), pct_very_poor = as.numeric(`PCT VERY POOR`),
         pct_fair = as.numeric(`PCT FAIR`), pct_good = as.numeric(`PCT GOOD`),
         pct_excellent = as.numeric(`PCT EXCELLENT`))%>%
  # remove old columns
  select(-c(`PCT POOR`, `PCT VERY POOR`, `PCT FAIR`, `PCT GOOD`, `PCT EXCELLENT`)) %>%
  # sum each row to check quality of data
  mutate(pct_sum = rowSums(dplyr::across(pct_poor:pct_excellent), na.rm = TRUE))

# find average pct per state per year in each category
cattle <- cattleNumeric %>%
  group_by(YEAR, STATE_NAME) %>%
  summarize(avg_poor = mean(pct_poor, na.rm = TRUE), avg_very_poor = mean(pct_very_poor, na.rm = TRUE), 
            avg_fair = mean(pct_fair, na.rm = TRUE), avg_good = mean(pct_good, na.rm = TRUE),
            avg_excellent = mean(pct_excellent, na.rm = TRUE)) %>%
  replace_na(list(avg_poor = 0, avg_very_poor = 0, avg_fair = 0, avg_good = 0, avg_excellent = 0))

# LOAD IN TEMPERATURE DATA
tmp <- readRDS("G:/Shared drives/AREC280F23/Data/Checkins/tmp_US_counties.rds")

# filter data to match states and years in cattle
tmp <- tmp %>%
  filter(ST_NAME %in% c("NEW MEXICO", "NORTH DAKOTA", "UTAH", "WEST VIRGINIA"),
         YEAR >= 2014, YEAR < 2023) %>%
  # group data by year and state, calculate average temperature per year per state
  group_by(YEAR, ST_NAME) %>%
  summarize(avg_tmp = mean(TMP, na.rm = TRUE),
            max_tmp = max(TMP),
            min_tmp = min(TMP))

# LOAD IN PRECIPITATION DATA and do the same as with tmp
pcp <- readRDS("G:/Shared drives/AREC280F23/Data/Checkins/pcp_US_counties.rds")

pcp <- pcp %>%
  filter(ST_NAME %in% c("NEW MEXICO", "NORTH DAKOTA", "UTAH", "WEST VIRGINIA"),
         YEAR >= 2014, YEAR < 2023) %>%
  group_by(YEAR, ST_NAME) %>%
  summarize(avg_pcp = mean(PCP, na.rm = TRUE),
            max_pcp = max(PCP),
            min_pcp = min(PCP))

# join tmp, pcp, and cattle
cattleWide <- tmp %>%
  left_join(pcp, by = c("YEAR", "ST_NAME")) %>%
  left_join(cattle, by = c("YEAR", "ST_NAME" = "STATE_NAME")) %>%
  mutate(change_tmp = avg_tmp - lag(avg_tmp),
         change_pcp = avg_pcp - lag(avg_pcp))

# pivot cattle longer
cattleLong <- cattleWide %>%
  pivot_longer(-c(YEAR, ST_NAME, avg_tmp, avg_pcp, change_tmp, change_pcp, max_tmp,
                  min_tmp, max_pcp, min_pcp), 
               names_to = "Condition", values_to = "Percent")

# PLOTTING
# cattle condition vs % in 2014 and 2022 | put side by side
cond2014 <- ggplot(cattleLong %>% filter(YEAR == 2014), 
       aes(Condition, Percent, fill = Condition)) +
  geom_col() +
  facet_wrap(~ ST_NAME) +
  labs(title = "Cattle Health Condition in 2014", x = "Health Condition") +
  scale_fill_manual(values = c("#28662b", "#2a8636", "#43aa47", "#81c953", "#97e589"),
                    labels = c("Excellent", "Fair", "Good", "Poor", "Very Poor")) + 
  theme(axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank(),
       legend.position = "right")

cond2022 <- ggplot(cattleLong %>% filter(YEAR == 2022),
       aes(Condition, Percent, fill = Condition)) +
  geom_col() +
  facet_wrap(~ ST_NAME) +
  labs(title = "Cattle Health Condition in 2022", x = "Health Condition") +
  scale_fill_manual(values = c("#28662b", "#2a8636", "#43aa47", "#81c953", "#97e589"),
                    labels = c("Excellent", "Fair", "Good", "Poor", "Very Poor")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")

# grid.arrange(cond2014, cond2022, nrow = 1, ncol = 2)

# max_tmp, max_pcp, min_tmp, min_pcp over time by state
ggplot(cattleLong, aes(YEAR, max_tmp)) +
  geom_line(aes(color = ST_NAME))

ggplot(cattleLong, aes(YEAR, max_pcp)) +
  geom_line(aes(color = ST_NAME))

ggplot(cattleLong, aes(YEAR, min_tmp)) +
  geom_line(aes(color = ST_NAME))

ggplot(cattleLong, aes(YEAR, min_pcp)) +
  geom_line(aes(color = ST_NAME))

# percent over time, by condition and state
ggplot(cattleLong, aes(YEAR, Percent, color = Condition)) +
  geom_line() + 
  facet_wrap(~ ST_NAME)

# pcp vs tmp
ggplot(cattleLong, aes(avg_tmp, avg_pcp)) +
  geom_point(aes(color = ST_NAME), size = 2.5) +
  geom_smooth(alpha = 0.5, color = "black") +
  labs(title = "Precipitation vs Temperature from 2014 to 2022", x = "Average Temperature (Degrees F)", 
       y ="Average Precipitation (Inches)")


# percent and temperature over time, by condition and state
ggplot(cattleLong, aes(x=YEAR)) +
  geom_line(aes(y= Percent, color = Condition), size = 0.85) + 
  geom_line(aes(y= max_pcp),color="black", size = 1.05)+ 
  scale_color_manual(values = c("#021893", "#94cbee", "#740699", "#ffcb13", "#d80027"),
                    labels = c("Excellent", "Fair", "Good", "Poor", "Very Poor")) + 
  scale_y_continuous(
      name = "Percent",
      sec.axis = sec_axis(~.* 1, name = "Precipitation (Inches)")
  )+
  facet_wrap(~ ST_NAME) +
  labs(title = "Cattle Health Condition Over Time", x = "Year")




