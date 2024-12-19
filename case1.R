#' Author: Ohm Kundurthy  
#' Date: Oct-04-2024
#' Purpose: OKCupid Case I Submission
-----------------------------------------------

######SAMPLE Step of SEMMA
install.packages("dplyr")
  
# Library loading
  
library(dplyr)
library(skimr)
library(ggplot2)
library(gridExtra)
library(plotly) 
library(leaflet)
library(ggmosaic)
library(stringr)
library(corrplot)
library(tidyr) 

# Set WD
setwd('~/Git/GitHub_R/Cases/Fall/I Ok Cupid')

# See all files in wd
dir()

# Get the okcupid data for `profiles`
profiles <- read.csv('profiles.csv')
latlon<- read.csv('LatLon.csv')
addr<- read.csv('addr.csv')
sharedCensus2010Vars<- read.csv("sharedCensus2010Vars.csv")

#Left Join latlon to profiles

profiles <- profiles %>%
  left_join(latlon, by = "location")

#--------------------------------------------------------------
######EXPLORE

# Get a summary of every column in df
skim(profiles)

# Group by status and ethnicity and count the occurrences
distinct_religions <- unique(profiles$religion)

# View the results
print(distinct_religions)


#review top 5 rows to understand data attributes in the df
colnames(profiles)
head(profiles,5)
head(latlon)
head(addr,5)
head(sharedCensus2010Vars,5)


# Review counts of profiles by sex
count_sex_plot <- ggplot(profiles, aes(x = sex)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Count of Profiles by Sex", 
       x = "Sex", 
       y = "Profile Count") +
  theme_minimal()

# Distribution of age by income 
# Create a histogram with facets for each sex
ggplot(profiles, aes(x = age, y = income, color = sex)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Income Distribution by Age and Sex",
    x = "Age",
    y = "Income",
    color = "Sex"  # This line adds a title to the legend
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "right") 


#Histogram to visualize the distribution of age across genders
ggplot(profiles, aes(x = age)) +
  geom_histogram(fill = "blue", bins = 30) +
  facet_wrap(~ sex) +
  labs(title = "age Distribution by Sex", x = "Age", y = "Count") +
  theme_minimal()

# Create a summary of counts by status and orientation
profile_summary <- profiles %>%
  group_by(status, orientation) %>%
  summarise(count = n(), .groups = 'drop')

# Create the bar chart
ggplot(profile_summary, aes(x = status, y = count, fill = orientation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Profiles by Status and Orientation",
    x = "Status",
    y = "Count",
    fill = "Orientation"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####### Conclusion Single Straight people are most common occurrence

profile_summary <- profiles %>%
  group_by(pets) %>%
  summarise(count = n(), .groups = 'drop')

# Create the bar chart
ggplot(profile_summary, aes(x = pets, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Profiles by pets ",
    x = "pets",
    y = "Count",
    fill = "Orientation"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###Offspring is "NA" or starts with "doesn't have kids" (most of the profiles)
###Pets shouldnt have word "has"


vegetarians <- profiles %>%
  filter(grepl("vegetarian", diet, ignore.case = TRUE))



ggplot(vegetarians) +
  geom_mosaic(aes(x = product(sex, sign_clean), fill = sign_clean)) +
  labs(title = "Mosaic Plot of Vegetarians by sex and Sign",
       x = "sex",
       y = "sign") +
  theme_minimal()


# Filter out rows where sign_clean is "unknown"
vegetarians_filtered <- vegetarians %>%
  filter(sign_clean != "unknown")

# Create the stacked bar plot
ggplot(vegetarians_filtered, aes(x = sign_clean, fill = sex)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Vegetarians by Sign and Gender",
       x = "Zodiac Sign",
       y = "Count of Vegetarians",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vegetarians_filtered, aes(x = sign_clean, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Vegetarians by Sign and Gender",
       x = "Zodiac Sign",
       y = "Count of Vegetarians",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#########Modify
# There are several data gaps and some that may be called as outliers (eg age > 100 and several profiles with income of $1M ) 
# There is not enough information to be sure these are outliers (from STDEV point of view) and hence I conclude its best to not modify / impute data 
# to avoid risk of further compromising the quality of data
#However, I have done some binning /bucketing into alphabetical or numeric categories to help with analysis

profiles <- profiles %>%
  mutate(religion_binned = case_when(
    str_detect(str_to_lower(religion), "christian") |  
      str_detect(str_to_lower(religion), "catholic") ~ "Christianity",
    str_detect(str_to_lower(religion), "islam") ~ "Islam",
    str_detect(str_to_lower(religion), "hindu") ~ "Hinduism",
    str_detect(str_to_lower(religion), "buddhism") ~ "Buddhism",
    str_detect(str_to_lower(religion), "judaism") ~ "judaism",
    TRUE ~ "Other"  # For any other values
  ))

profiles <- profiles %>%
  mutate(religion_binned2 = case_when(
    str_detect(str_to_lower(religion), "serious") ~ "Serious",
    str_detect(str_to_lower(religion), "laughing") ~ "Casual",
    TRUE ~ "Other"  # For any other values
  ))

profiles <- profiles %>%
  mutate(sign_clean = case_when(
    str_detect(str_to_lower(sign),"aries") ~ "Aries"	,
    str_detect(str_to_lower(sign),"taurus") ~ "Taurus"	,
    str_detect(str_to_lower(sign),"gemini") ~ "Gemini"	,
    str_detect(str_to_lower(sign),"cancer") ~ "Cancer"	,
    str_detect(str_to_lower(sign),"leo") ~ "Leo"	,
    str_detect(str_to_lower(sign),"virgo") ~ "virgo"	,
    str_detect(str_to_lower(sign),"libra") ~ "Libra"	,
    str_detect(str_to_lower(sign),"scorpio") ~ "Scorpio"	,
    str_detect(str_to_lower(sign),"sagittarius") ~ "Sagittarius"	,
    str_detect(str_to_lower(sign),"sapricorn") ~ "Capricorn"	,
    str_detect(str_to_lower(sign),"aquarius") ~ "Aquarius"	,
    str_detect(str_to_lower(sign),"pisces") ~ "Pisces"	,
    TRUE ~ "unknown"  # For any other values 
  ))

profiles <- profiles %>%
  mutate(CatPerson = case_when(
    str_detect(str_to_lower(pets),"has cat") | str_detect(str_to_lower(pets),"likes cat") ~ 1	,
    TRUE ~ 0  # For any other values 
  ))

profiles <- profiles %>%
  mutate(DogPerson = case_when(
    str_detect(str_to_lower(pets),"has dog") | str_detect(str_to_lower(pets),"likes dog") ~ 1	,
    TRUE ~ 0  # For any other values 
  ))


profiles <- profiles %>%
  mutate(HighIncome = case_when(
    income >=100000 ~ 1	,
    TRUE ~ 0  # For any other values 
  ))

profiles <- profiles %>%
  mutate(HighIncome = case_when(
    income >=100000 ~ 1	,
    TRUE ~ 0  # For any other values 
  ))

profiles <- profiles %>%
  mutate(Age_Bin = cut(age, breaks = seq(0, 100, by = 5), right = FALSE))  # Create 5-year age bins

########Model Personas
#A lot of thought and data exploratin went into creation of these personas 
#But the code only shows the final four personas chosen to be included in the case 

#Persona 1  - Young Employed - Criteria Age <=40 and Job Not NULL and Income > 0 
YoungProf <- profiles %>%
  filter(age <= 40, !is.na(job), income > 0)


#Bar chart to show average income by Gender and Body Type
YoungProf %>%
  group_by(sex, body_type) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = body_type, y = avg_income, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Income by Gender and Body Type", 
       x = "sex", 
       y = "Average Income",
       fill = "Body Type") +
  theme_minimal()

#############
# YoungProf is your data frame with columns 'sex', 'body_type', and 'profiles'
# Group by 'sex' and 'body_type' and count the number of profiles in each group
data_summary <- YoungProf %>%
  group_by(sex, body_type) %>%
  summarise(profile_count = n()) %>%
  ungroup()
# Create a grouped bar plot to visualize the counts
ggplot(data_summary, aes(x = body_type, y = profile_count, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Profiles by Sex and Body Type",
       x = "Body Type",
       y = "Profile Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")  # Optional: for a nice color palette
##Most of the men are athletic, fit or average
##women are curvy or average


# Create the stacked bar plot
ggplot(YoungEmployed, aes(x = Age_Bin, fill = sex)) +
  geom_bar(position = "stack") +
  facet_wrap(~ orientation) +
  labs(title = "Count of Profiles by Orientation and Sex",
       x = "Age Bins (5-Year Intervals)",
       y = "Count of Profiles",
       fill = "Sex") +
  theme_minimal()
## Most people are Straight and count of Females to Male is low across age groups 
## that may lead to resentment in users long term as they may not be able 
## to find a match

ggplot(YoungEmployed, aes(x = age, fill = sex)) +
  geom_bar(position = "stack") +  # Stacked bars
  facet_wrap(~ orientation) +      # Faceting by Orientation
  labs(title = "Distribution of Orientation by Age and Sex",
       x = "Age",
       y = "Counts",
       fill = "Sex") +
  theme_minimal() +               # Minimal theme for aesthetics
  scale_fill_manual(values = c("m" = "blue", "f" = "pink")) # Custom colors

#######Persona 2  - Free Spirits

freeSpirit <- profiles %>%
  filter(
    (is.na(offspring) | grepl("^doesn't have kids", offspring)) &  # Offspring is NA or starts with "doesn't have kids"
      !grepl("has", pets, ignore.case = TRUE) &  # Pets column shouldn't have the word "has"
     # orientation == "straight" &  # Orientation is straight
      status == "single"  # Status is single
  )

# Create a bar chart for the distribution of the 'drinks' column
ggplot(freeSpirit, aes(x = drinks)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Drinks Preferences",
    x = "Drinks",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
### Frequent drinkers and Social Drinkers are the most common among profiles

# Filter out rows with missing latitude or longitude
freeSpirit_clean <- freeSpirit %>%
  filter(!is.na(lat) & !is.na(lon))

####### freeSpirit has columns 'lat' for latitude and 'long' for longitude
# Create the leaflet map
leaflet(data = freeSpirit) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~lat, 
    lng = ~lon, 
    radius = 5, 
    color = "blue", 
    stroke = FALSE, 
    fillOpacity = 0.6,
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = mean(freeSpirit$lon), lat = mean(freeSpirit$lat), zoom = 2)
#################### only 5 profiles outside usa and Not much presence outside sf bay area in usa  

JobLessAtMidlife <- profiles %>%
  filter(  # Filter for midlife (age between 40 and 60)
      (is.na(job) | job == "unemployed") &  # Job is either blank (NA) or "unemployed"
      income > 0
  )

# Create a bar chart for the distribution of the 'drinks' column
ggplot(freeSpirit, aes(x = drinks)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Drinks Preferences",
    x = "Drinks",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
### Frequent drinkers and SocialDrinkers are the most common profiles


#####Persona 3 Students 
students <- profiles %>%
  filter(grepl("working", education, ignore.case = TRUE))

ggplot(students, aes(x = diet)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Diet Preferences Among Students",
       x = "Diet",
       y = "Count of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# dont care about Diet 

ggplot(students, aes(x = as.factor(income))) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Income Distribution Among Students",
       x = "Income",
       y = "Count of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#dont have money 

# Group by location and count students in each location
location_counts <- students %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  filter(count > 200)

head(location_counts)



#####Persona 4 PetPersons 

# Create a summary table counting occurrences of sex
distribution_summary <- profiles %>%
  group_by(HighIncome, sex) %>%
  summarize(Count = n(), .groups = 'drop')

# Create the bar plot
ggplot(distribution_summary, aes(x = factor(HighIncome, labels = c("Not High Income", "High Income")), 
                                 y = Count, 
                                 fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Sex by High Income Status",
       x = "Income Status",
       y = "Count",
       fill = "Sex") +
  theme_minimal()
############

# Create a summary table for CatPersons grouped by status
catperson_summary <- profiles %>%
  group_by(status) %>%
  summarize(
    CatPerson_Count = sum(CatPerson),  # Count of CatPersons
    NonCatPerson_Count = sum(1 - CatPerson)  # Count of Non-CatPersons
  ) %>%
  pivot_longer(cols = c(CatPerson_Count, NonCatPerson_Count),
               names_to = "PetType",
               values_to = "Count") %>%
  mutate(PetType = ifelse(PetType == "CatPerson_Count", "Cat Person", "Non Cat Person"))  # Rename for clarity

# Create a summary table for DogPersons grouped by status
dogperson_summary <- profiles %>%
  group_by(status) %>%
  summarize(
    DogPerson_Count = sum(DogPerson),  # Count of DogPersons
    NonDogPerson_Count = sum(1 - DogPerson)  # Count of Non-DogPersons
  ) %>%
  pivot_longer(cols = c(DogPerson_Count, NonDogPerson_Count),
               names_to = "PetType",
               values_to = "Count") %>%
  mutate(PetType = ifelse(PetType == "DogPerson_Count", "Dog Person", "Non Dog Person"))  # Rename for clarity

# Combine the two summaries
combined_summary <- bind_rows(
  catperson_summary %>% mutate(Pet = "Cat"),
  dogperson_summary %>% mutate(Pet = "Dog")
)

# Create the bar plot with facets
ggplot(combined_summary, aes(x = factor(status), 
                             y = Count, 
                             fill = PetType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ Pet) +  # Create separate panels for Cat and Dog
  labs(title = "Distribution of Cat and Dog Persons by Status",
       x = "Status",
       y = "Count",
       fill = "Pet Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate counts of DogPersons and Not DogPersons by age bin
age_distribution <- profiles %>%
  group_by(Age_Bin) %>%
  summarize(
    DogPerson_Count = sum(DogPerson),  # Count of DogPersons
    NotDogPerson_Count = sum(1 - DogPerson),  # Count of Not DogPersons
    .groups = 'drop'  # Drop grouping after summarizing
  ) %>%
  pivot_longer(cols = c(DogPerson_Count, NotDogPerson_Count),
               names_to = "PetStatus",
               values_to = "Count")

# Create the stacked bar plot for DogPerson vs Not DogPerson by age
ggplot(age_distribution, aes(x = Age_Bin, y = Count, fill = PetStatus)) +
  geom_bar(stat = "identity") +
  labs(title = "Age Distribution of DogPersons and Not DogPersons",
       x = "Age Bins",
       y = "Count",
       fill = "Pet Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######
# End




