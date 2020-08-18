# loading packages
library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)

# loading the monster_jobs dataset
covid_df <- read_csv("C:/Users/User/Desktop/Monash/R Program/COVID/covid19.csv")

#Analyse the data->Know row&col;Know whether any weird miscorrect info;datatype
glimpse(covid_df)
dim(covid_df)

#Vector Data Structure
vector_cols <- colnames(covid_df)
vector_cols

#Top few records
head(covid_df)

#Filter only by country level - inconsistant
covid_df_all_states <- covid_df %>%
  filter(Province_State == 'All States') %>%
  select(-Province_State)


#We shouldn't compare column with cumulative content and daily day-to-day content
#Need to standardize - so let's work with daily data
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)


#------------------Top 10 records
# 1. How can we get the overall number of COVID-19 tested, positive, 
#active and hospitalized cases by country since we currently have daily data? - Group
# 2. How do we then extract the top ten? - Summarize
# 3. Arrange() desc and head() for top ten

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarise(tested = sum(daily_tested),
            positive = sum(daily_positive),
            active=sum(active),
            hospitalized =sum(hospitalizedCurr)) 

#Arrange and top 10
covid_df_all_states_daily_sum <- covid_df_all_states_daily_sum %>%
  arrange(-tested)

#Top 10
covid_top_10 <- head(covid_df_all_states_daily_sum,10)



# Which countries have had the highest 
#number of positive cases against the number of tests?
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

names(tested_cases) <- c(countries)
names(positive_cases) <- c(countries)
names(active_cases) <- c(countries)
names(hospitalized_cases) <- c(countries)

#Top three against positive
positive_tested_top_3 <-  positive_cases / tested_cases
positive_tested_top_3 <-  sort(positive_tested_top_3,decreasing = T)[1:3]

#Final Result - Find another way
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

covid_mat <-  rbind(united_kingdom,united_states,turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

covid_mat

#Final Solution
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

dataframe_list <- list(covid_df, covid_df_all_states, covid_df_all_states_daily,covid_top_10)
matrix_list <- list(covid_mat)
vector_list <- list(vector_cols, countries)

data_structure_list <- list(dataframe_list,vector_list,matrix_list)

covid_analysis_list <- list(question,answer,data_structure_list)

covid_analysis_list[[2]]
