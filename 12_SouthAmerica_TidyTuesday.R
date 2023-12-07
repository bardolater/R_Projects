#  South America Map for 30DayMapChallenge
#


library(tidyverse)
library(scales)
library(viridis)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
font_add_google("Roboto Condensed")
font <- "Roboto Condensed"


# Data file is COMPACT file downloaded from https://population.un.org/wpp/Download/Standard/MostUsed/
df <- readxl::read_xlsx("./Data/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1_copy.xlsx", skip = 16)
# South America shapefile from: https://earthworks.stanford.edu/catalog/stanford-vc965bq8111 
south_america <- read_sf("./Data/stanford-vc965bq8111-shapefile/vc965bq8111.shp")
south_america <- south_america |> mutate(name = ifelse(name=="Bonaire (NETH)", "BONAIRE", name))
south_america <- south_america |> mutate(name = ifelse(name=="Curacao (NETH)", "CURAÇAO", name))
south_america <- south_america |> mutate(name = ifelse(name=="Falkland Islands (Islas Malvinas) (UK)", "FALKLAND ISLANDS (MALVINAS)", name))
south_america <- south_america |> mutate(name = ifelse(name=="French Guiana (FRANCE)", "FRENCH GUIANA", name))
ggplot() +
  geom_sf(data=south_america, aes(fill=name)) +
  theme_void()
# Clean variable name and re-name countries to match name in map data
df <- df |> rename(Country = `Region, subregion, country or area *`,
                   iso_a3 = `ISO3 Alpha-code`,
                   Life_Expectancy = `Life Expectancy at Birth, both sexes (years)`)
df <- df |> mutate(Country = (ifelse(Country=="Bolivia (Plurinational State of)", "Bolivia", Country)))
df <- df |> mutate(Country = (ifelse(Country=="Venezuela (Bolivarian Republic of)", "Venezuela", Country)))
df <- df |> mutate(name = str_to_upper(Country))

distinct_countries <- df |> select(name) |> group_by(name) |> arrange(name) |> distinct()
#Not matched are Curacao, In dispute
# Wrangle data to combine map information with data about life expectancy in each country
list_countries <- south_america$name
codes <- data.frame(name = list_countries)
codes <- codes |> distinct()
df_v2 <- inner_join(df, codes)
df_v3 <- df_v2 |> filter( Year==2021)
df_v4 <- df_v3 |> dplyr::select(name, Year, Life_Expectancy) |> 
  mutate(Life_Expectancy = as.numeric(Life_Expectancy))  
map_LifeExpectancy <- left_join(south_america, df_v4)

#Obtain the USA life expectancy change
USA <- df |> dplyr::select(name, Year, Life_Expectancy) |> 
  filter(name=="UNITED STATES OF AMERICA") |> 
  filter(Year==1950 | Year==2021) |> 
  group_by(name) |> 
  mutate(Life_Expectancy = as.numeric(Life_Expectancy), Percent = (Life_Expectancy - lag(Life_Expectancy)) / lag(Life_Expectancy) ) |> 
  ungroup() |> 
  filter(!is.na(Percent))

ggplot()+
  geom_sf(data=map_LifeExpectancy, aes(fill=Life_Expectancy)) +
  scale_fill_gradientn(colours=rev(magma(6)),
                       name="Life Expectancy") +
  coord_sf() +
  theme_void() +
  labs(title= "Average Life Expectancy in South America (2021)",
      subtitle = "For reference, life expectancy in USA was 77 in 2021",
       caption = "Source: World Population Prospects 2022 |  #TidyTuesday | Map by @DataAngler@vis.social") +
  theme(plot.title = element_text(family = font, hjust = 0.5, size = 11),
    plot.subtitle =  element_text(family = font, hjust = 0.5, size=10),
    plot.caption = element_text(family = font, hjust = 0.5, size = 10),
    legend.text = element_text(family = font, size=10),
    legend.title = element_text(family = font, size=10),
    legend.position = "top")
ggsave("./Charts/2021_SouthAmerica_LifeExpectancy.png", dpi=300, height=7, width = 6.8, units="in", bg="beige")
                                  
        


