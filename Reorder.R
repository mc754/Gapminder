# load gapminder data
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)

# reorder by median income and color by continent
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365, 
         region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + 
  scale_y_continuous(trans = "log2") + 
  geom_point(show.legend = FALSE)