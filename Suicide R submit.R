library(tidyverse)
# colour code:
# https://html-color-codes.info/colors-from-image/?imageLoader=world_map_rate2019


### preparation ###
## annualSuicideRate
# read csv
annualSuicideRate <- read.csv("/Users/rain/Documents/D.V. Coursework/suicide/suicide-death-rates.csv", header = TRUE)
# rename column by index
colnames(annualSuicideRate)[4] = "suicide_rate"
#add new column that cuts 'points' into categories
annualSuicideRate$cat <- cut(annualSuicideRate$suicide_rate,
              breaks=c(0, 5, 10, 15, 20, 25, 100),
              labels=c('<5', '5-10', '10-15', '15-20','20-25', '>25'))
# filter year 2019
suicideRate2019 <- filter(annualSuicideRate, Year == "2019")
suicideRate1990 <- filter(annualSuicideRate, Year == "1990")
## WDI
# install.packages("WDI")
library(WDI)
new_wdi_cache <- WDIcache()
wdi_country <- new_wdi_cache$country
# join
suicide_wdi <- left_join(annualSuicideRate, wdi_country,
                         by = c("Code" = "iso3c")) 
# suicide_wdi <- left_join(suicide_wdi, mental, by = c("Code" = "Code")) 
suicide_wdi <- drop_na(suicide_wdi)#
suicide_wdi2019 <- filter(suicide_wdi, Year == "2019")
suicide_wdi1990 <- filter(suicide_wdi, Year == "1990")

## death cases
deathCases <- read.csv("/Users/rain/Documents/D.V. Coursework/suicide/annual-number-of-deaths-by-cause.csv", header = TRUE)
colnames(deathCases)[20] = "self_harm"
suicideCases <- subset(deathCases, select = c("Code", "Year", "self_harm"))
suicideCases2019 <- filter(suicideCases, Year == "2019")
suicideCases1990 <- filter(suicideCases, Year == "1990")

## suicide world gender
view(worldSuicideGender)
SuicideGender <- read.csv("/Users/rain/Documents/D.V. Coursework/suicide/male-vs-female-suicide.csv", header = TRUE)
worldSuicideGender <- filter(worldSuicideGender, Entity == "World")
colnames(worldSuicideGender)[4] = "male"
colnames(worldSuicideGender)[5] = "female"
# melt and cast
library(reshape)
# reorder the rows
worldSuicideGender1 <- melt(worldSuicideGender, id.vars='Year')

## suicide world age
view(worldSuicideAge)
SuicideAge <- read.csv("/Users/rain/Documents/D.V. Coursework/suicide/suicide-deaths-by-age.csv", header = TRUE)
worldSuicideAge <- filter(SuicideAge, Entity == "World")
colnames(worldSuicideAge)[4] = "age_70+"
colnames(worldSuicideAge)[5] = "age_50-69"
colnames(worldSuicideAge)[6] = "age_15-49"
colnames(worldSuicideAge)[7] = "age_5-14"


### visualisation ###
## plot the shapefile
library(tmap)
library(rgdal)
# worldshape download from:
# https://www.naturalearthdata.com/features/
worldShape<-readOGR(dsn="./ne_110m_admin_0_countries", "ne_110m_admin_0_countries")
sf_use_s2(FALSE)
qtm(worldShape)
# set mode
tmap_mode("view") #
tmap_mode("plot") #
#1 join suicide2019 data with the map
worldShape@data<-left_join(worldShape@data, suicideRate2019, 
                               by=c('ADM0_A3'='Code'))
# plot the map
# install.packages("viridis")
# library(viridis)
qtm(worldShape, fill="suicide_rate", 
    fill.palette= c("Reds"),fill.breaks=c(0,10,20,30,40,50,60,70,80,90,100))
#2 join suicide1990 data with the map
worldShape@data<-left_join(worldShape@data, suicideRate1990, 
                           by=c('ADM0_A3'='Code'))
# plot the map
qtm(worldShape, fill="suicide_rate", 
    fill.palette= "Reds", fill.breaks=c(0,10,20,30,40,50,60,70,80,90,100))

## plot a cartogram
#1# suicide rate
# documentation:
# https://cran.r-project.org/web/packages/cartogram/readme/README.html
library(cartogram)
devtools::install_github("sjewo/cartogram") #
library(sf)
# suicideCarto<- cartogram_cont(worldShape, weight="suicide_rate", itermax=10, prepare="adjust") #error
# error solved from this website:
# https://gis.stackexchange.com/questions/346075/trying-to-create-a-cartogram-and-the-st-transform-function-does-not-work
worldShape_sf <- st_as_sf(worldShape)
afr_sf_proj = st_transform(worldShape_sf,3857)
suicideCarto<- cartogram_cont(afr_sf_proj, weight="suicide_rate", itermax=15,
                              prepare="adjust") 
# plot it
tm_shape(suicideCarto) +
  tm_fill("suicide_rate", style="jenks") + 
  tm_borders() + tm_layout(frame=F)
#2# cartogram of suicide cases
worldShape@data<-left_join(worldShape@data, suicideCases1990,
                           by=c('ADM0_A3'='Code'))
worldShape_sf <- st_as_sf(worldShape)
afr_sf_proj = st_transform(worldShape_sf,3857)
suicideCarto<- cartogram_cont(afr_sf_proj, weight="self_harm", itermax=15,
                              prepare="adjust")
# plot it
tm_shape(suicideCarto) +
  tm_fill("self_harm", style="jenks") +
  tm_borders() + tm_layout(frame=F)

## world stacked chart by age and year
# melt
data <- data.frame(Entity,year_1,year_2,year_3) 
worldSuicideAge_melt <- melt(worldSuicideAge, id = c("Year")) 
worldSuicideAge_melt <- worldSuicideAge_melt[!worldSuicideAge_melt$variable == "Code", ]
# plot it
ggplot(worldSuicideAge_melt, aes(x=Year, y=value_nu, fill=variable)) +
  geom_area(alpha=0.8)+
  labs(title = "Suicide Mortality Cases by Age Group in 1990-2019", 
       x = "Year", 
       y = "Suicide Mortality Cases")+
  theme_minimal()+
  scale_fill_manual(values = c("age_70+" = "#4058AE", "age_50-69" = "#6A9BC3",
                               "age_15-49" = "#98D7C4", "age_5-14" = "#F3B942"))+
  theme(plot.title = element_text(hjust = 0.5))

## double side bar chart year and gender
library(reshape2)
worldSuicideGender <- subset(worldSuicideGender, select=c("Year","male","female"))
# melt
worldSuicideGender1 <- melt(worldSuicideGender, id.vars='Year')
colnames(worldSuicideGender1)[2] = "Gender"
# plot it
ggplot(worldSuicideGender1, aes(x=Year, y=value, fill=Gender)) +
  geom_bar(stat='identity', position='dodge')+
  labs(title = "Suicide Mortality Rate by Gender in 1990-2019", 
       x = "Year", 
       y = "Suicide Mortality Rate (per 100,000 capita)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("legend", 
                    values = c("male" = "#1F8FFF", "female" = "#F98072"))
# alternative line #
ggplot(worldSuicideGender1, aes(year, x=Year, y=value, colour=Gender)) + 
  geom_line()+
  labs(title = "Suicide Mortality Rate by Gender in 1990-2019", 
       x = "Year", 
       y = "Suicide Mortality Rate (per 100,000 capita)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("legend", 
                    values = c("male" = "#1F8FFF", "female" = "#F98072"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40))

## scatter plot grid by income
suicide_wdi <- filter(suicide_wdi, income == c("High income", 
                                              "Upper middle income",
                                              "Lower middle income",
                                              "Low income"))
ggplot(suicide_wdi,aes(Year, suicide_rate) ) + 
  geom_point(aes(colour = region)) + 
  scale_colour_manual(values = c("#FDA200",
                               "#00B7EC",
                               "#E47FAF",
                               "#F5E736",
                               "#0076B8",
                               "#F5630D",
                               "#00A276"))+
  facet_grid(.~ factor(income, levels=c("High income", 
                                      "Upper middle income",
                                      "Lower middle income",
                                      "Low income"))) +
  labs(x="Year", y="Suicide Mortality Rate (per 100,000 capita)",
       title = "Suicide Mortality Rate by Income Level") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.spacing.x = unit(5, "mm"))
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

   
  
