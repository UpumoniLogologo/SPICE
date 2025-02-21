##download data 

#Tried but did not work ----
#url_tcc_upper <- "https://apps.cbpp.org/program_participation/php/data.php?action=getSpreadsheetByID&table_id=367"
#download.file(url_tcc_upper, "tcc_upper_chamber.xlsx", 'curl')
#url_tcc_lower <- "https://apps.cbpp.org/program_participation/php/data.php?action=getSpreadsheetByID&table_id=368"
#download.file(url_tcc_upper, "tcc_lower_chamber.xlsx", 'curl')
#url_children <- "https://apps.cbpp.org/program_participation/php/data.php?action=getSpreadsheetByID&table_id=369"
#download.file(url_tcc_upper, "ctc_children.xlsx", 'curl')

##Creating maps using state and house districts for Poster ----

tcc_upper <- readxl::read_xlsx("tcc_upper_chamber.xlsx")
head(tcc_upper)
tcc_upper[1:20,]
tcc_lower <- readxl::read_xlsx("tcc_lower_chamber.xlsx")
head(tcc_lower)
tcc_lower[1:20,]
ctc_children <- readxl::read_xlsx("ctc_children.xlsx")
head(ctc_children)
ctc_children[1:20,]
summary(ctc_children)

View(tcc_upper)
tcc_upper[455:480,]
ctc_children[18,] 

tcc_upper_hi <- tcc_upper[7:2088,]
head(tcc_upper_hi)
names(tcc_upper_hi)<- c("State", "uTaxFilers", "uNumFilers_EITC", "uPerFilers_EITC","uAmount_EITC", 
                        "uNumFilers_RPEITC", "uAmount_RPEITC", "uNumFilers_NRCTC",
                        "uPerFilers_NRCTC", "uAmount_NRCTC", "uNumFilers_RACTC", 
                        "uPerFilers_RACTC", "uAmount_RACTC","uTotal_CTC_ACTC", "uPerDistPop")

names(tcc_upper_hi)

tcc_lower_hi<- tcc_lower[7:2088,]
head(tcc_lower_hi)
names(tcc_lower_hi)<- c("State","LTaxFilers", "LNumFilers_EITC", "LPerFilers_EITC", "LAmount_EITC",
                        "LNumFilers_RPEITC", "LAount_RPEITC", "LNumFilers_NRCTC", 
                        "LPerFilers_NRCTC", "LAmount_NRCTC", "LNumFilers_RACTC", 
                        "LPerFilers_RACTC", "LAmount_RACTC", "LTotal_CTC_ACTC", "LPerDistPop")

names(tcc_lower_hi)
head(tcc_lower_hi)

ctc_children_hi <- ctc_children [7:58,]
head(ctc_children_hi)
names(ctc_children_hi) <- c("State", "u18", "u17", "u6", "Living_Rural", "SNAP_Households", 
                            "White_NLatino", "Black_NLatino", "Latino", "Asian_NLatino", 
                            "Other_Mult_NLatino", "Not_US_Citizen")

names(ctc_children_hi)
head(ctc_children_hi)

library(dplyr)
library(stringr)

tcc_upper_hi <- tcc_upper_hi %>% 
  filter(str_detect(State,
regex("Hawa", ignore_case= TRUE)))

#Install Packages ----
install.packages("sf")
install.packages("leaflet")
install.packages("tidyverse")
install.packages("tidyr")

#Load Libraries ----
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)

#Reading in CSV File ----
censusdata <- read.csv("~/Desktop/SPICE/SPICE_2023/CensusData.csv")
View(censusdata)

#Splitting Column Into Three For a Common ----
censusdata <- censusdata %>% separate(NAME,
                                      into = c("NAMELSAD", "county", "state"),
                                      sep = ",")
View(censusdata)

censusdata <- as_tibble(censusdata)

#Subsetting the Data ----
censusdata2 <- censusdata %>%
  select(NAMELSAD, household_est, annot_est, mc_household_total, mc_household_u18, cc_household_total, cc_household_total_u18,
         male_household_total, male_household_total_u18, male_household_total_alone, male_household_total_alone_o65, female_household_total,
         female_household_total_u18, female_household_total_alone, female_household_total_alone_o65, total_household_u18, total_household_o65,
         total_household_avg, total_household_fam, pop_household, span_lang, indoe_lang, api_lang, total_pop, american_pop, arab_pop, czech_pop,
         danish_pop, dutch_pop, english_pop, french_pop, french_canadian_pop, german_pop, greek_pop, hungarian_pop, irish_pop, italian_pop,
         lithuanian_pop, norwegian_pop, polish_pop, portuguese_pop, russian_pop, scotch_irish_pop, scottish_pop, slovak_pop, subsaharan_african_pop,
         swedish_pop, swiss_pop, ukrainian_pop, welsh_pop, west_indian_pop)
View(censusdata2)

#Inner Join Census Data with Shapefile ----
acs_tracts_2021 <- inner_join(hi_tracts, censusdata2, by = "NAMELSAD")
View(acs_tracts_2021)

names(hi_tracts)



#Read shape files and creating a a map ----
state_house <- read_sf(dsn = "./Hawaii_State_House_Districts_2012", 
                       layer = "Hawaii_State_House_Districts_2012")
state_senate<- read_sf(dsn = "./Hawaii_State_Senate_Districts_2012",
                       layer = "Hawaii_State_Senate_Districts_2012")

# map shapes
m <- leaflet(state_senate)%>% 
  setView(lng = -157.6710942,
          lat = 20.9621706, 
          zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(weight = 1, 
              color= '#000000',
              fillOpacity = 0.5)
m  

m2 <- leaflet(state_house)%>% 
  setView(lng = -157.6710942,
          lat = 20.9621706, 
          zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(weight = 1, 
              color= 'Blue',
              fillOpacity = 0.5)

m2


#Different datasets with census tracks ----
hi_tracts <- read_sf(dsn = ".", 
                     layer = "hi_tracts")
# split column
income_local <- read.csv("Income by Location.csv")
View(income_local)
# splitting column into three for a common
income_local <- income_local %>% separate(Geography, 
                                          into = c("NAMELSAD", "county","state"),
                                          sep = ",")
     
library(ggplot2)    
         

## Creating Official Visuals for Poster----

oahu <- read.csv("~/Desktop/SPICE/SPICE_2023/Island of O'ahu Racial Makeup - Racial Makeup.csv")
View(oahu)
oahu1 <- oahu[-c(7:9), ]
View(oahu1)
oahu2 <- ggplot(data = oahu1, aes(x=Race, y=Population, fill=Race, )) +
  geom_bar(stat = "identity") +
  labs(title = "Census Racial Makeup on the Island of O'ahu") +
  theme(axis.text.x=element_text(angle=90))
oahu2
oahu3 <- oahu[-c(1:6, 9), ]
View(oahu3)
oahu4 <- ggplot(data = oahu3, aes(x=Race, y=Population, fill=Race, )) +
  geom_bar(stat = "identity") +
  labs(title = "Identification of One Race or Two or More") +
  theme_minimal()
oahu4


income_oahu <- read.csv("~/Desktop/SPICE/SPICE_2023/Island of O'ahu Income Data - Income.csv")
View(income_oahu)
income_oahu1 <- income_oahu[-c(11), ]
View(income_oahu1)
income_oahu1$Income <- factor(income_oahu1$Income,
                              levels = c("Less than $10,000", "$10,000 to $14,999",
                                         "$15,000 to $24,999", "$25,000 to $34,999",
                                         "$35,000 to $49,999", "$50,000 to $74,999",
                                         "$75,000 to $99,999", "$100,000 to $149,999",
                                         "$150,000 to $199,999", "$200,000 or more"))
income_oahu2 <- ggplot(data = income_oahu1, aes(x=Income, y=Population, fill=Income, )) +
  geom_bar(stat = "identity") +
  labs(title = "Income Data for the Island of O'ahu") +
  theme(axis.text.x=element_text(angle=90))
income_oahu2



