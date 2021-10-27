#Pie Charts for Site Descriptions

# Import Libraries --------------------------------------------------------

library("ggplot2")
library("tidyverse")
library("scales")


# Data Import -------------------------------------------------------------

# Set this value to your desired site:
site_id <- "02-015"


# Load comp site data
Comp <- read.csv("./Results/2021/VegDataResults_2021.csv", fileEncoding = "UTF-8-BOM")  %>% 
  subset(Site_ID == site_id)


# Data Management ---------------------------------------------------------

# convert to long-format for pie plotting the chart.
CompLong <- Comp %>%
  select(c("n_ra", "e_ra", "i_ra", "u_ra")) %>%
  transmute(Native = n_ra, Exotic = e_ra, Invasive = i_ra, Unknown = u_ra) %>%
  gather(Origin, Percent_Cover, Native:Unknown, factor_key=TRUE) %>%
  arrange(-Percent_Cover) %>%
  mutate(Percent_Cover_cs = cumsum(Percent_Cover))


# Create the figure -------------------------------------------------------

# pie chart
chart1 <- ggplot(CompLong, aes(x = "", y = Percent_Cover, fill = Origin)) +
  geom_col()
chart1
piechart <- chart1 + coord_polar("y", start = 0, direction = 1)
piechart

# Create Blank Theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=15, face="bold")
  )

# Final version of pie chart with blank theme and blue, colour-blind-friendly theme
piechart + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank(), 
        legend.title = element_blank(),
        legend.text= element_text(size=14))+
  geom_text(aes(x = 1, y = Percent_Cover_cs - Percent_Cover/2,
                label = percent(Percent_Cover/100, accuracy = 0.1)),
            position = position_dodge(width = 0.5), size = 5)
