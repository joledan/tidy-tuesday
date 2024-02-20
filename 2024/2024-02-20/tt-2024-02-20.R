# title: tt-2024-02-20
# date: february 20, 2024
# by: jan oledan
# desc: tidy tuesday challenge - valentines day consumer spending

##### install and load packages #####
rm(list=ls())

# package names
library(tidyverse)
library(camcorder)
library(janitor)
library(tidytuesdayR)
library(ggtext)
library(showtext)

##### set up directories ######
main <- "tidy-tuesday/2024/2024-02-20"
plots <- "plots"


### intialize camcorder to set up and output square plot ###
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 1,
  width = 1050,
  height = 1050,
  units = "px",
  dpi = 300,
  bg = "white"
)



##### define main ggplot theme components #####
th <- theme_minimal(base_family = "Noto Sans") + 
  theme(legend.position = "none") +
  theme(panel.border = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.title = element_text(face = "bold", # plot title, subtitle, caption
                                  size = 12,
                                  margin = margin(t=0, r=0, b=2, l=0, "pt")),
        plot.subtitle = element_text(size = 10,
                                     margin = margin(t=0,r=0,b=4,l=0, "pt")),
        plot.caption = element_markdown(hjust = 0,
                                        size = 6,
                                        color = "#999999",
                                        margin = margin(t=4,r=0,b=0,l=0, "pt"),
                                        lineheight = 1.2),
        plot.caption.position = "plot") +
  theme(axis.title = element_blank(),
        axis.title.x = element_blank(), # adjust x axis title
        axis.title.y = element_blank(), # adjust y axis title
        axis.text.x = element_text(size = 8, # make axis text (labels) size 8
                                   colour = "#000000"),
        axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = -.5,
                                   hjust = 1),
        axis.ticks.length.x = unit(4, "pt"), # define tick length and colour for x-axis
        axis.ticks.x = element_line(colour="#000000"),
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.line.x = element_line(colour = "#000000"), # adjust x axis line
        panel.grid.major.x = element_blank(), # remove major x lines
        panel.grid.minor.x = element_blank(), # remove minor x lines
        panel.grid.minor.y = element_blank(), # remove minor y lines
        axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = -16, unit = "pt"))) # adjust tick length to go on top of line

##### read data #####
tuesdata <- tidytuesdayR::tt_load('2024-02-20')

isc_grants <- tuesdata$isc_grants

##### basic line chart - funding over time #####

# split by year
df <- isc_grants %>%
  group_by(year) %>%
  summarize(funded = sum(funded)/1000,
            n = n())

# by year, group
df1 <- isc_grants %>%
  group_by(year, group) %>%
  summarize(funded = sum(funded)/1000,
            n = n())


##### plot here #####
f1 <- ggplot(data = df, 
             aes(y = funded,
                 x = year)) +
  th +
  geom_bar(stat = "identity",
           fill = "#0072B2") +
  scale_x_continuous(breaks = seq(2016, 2023, 2),
                     expand = expansion(mult = c(0, 0.1),
                                        add = c(0, 0))) +
  scale_y_continuous(labels = c("    0", "100", "200", "300"),
                     position = "right",
                     expand = expansion(mult = c(0, 0.1),
                                        add = c(0, 0))) +
  labs(title = "Title",
       subtitle = "Amount awarded, thousands USD",
       caption = "**Source:** R Consortium ISC, TidyTuesday Week 8 2024 <br> **Visual:** Jan Oledan")

f1
  

# make gif!
# gg_playback(
#   name = file.path(paste(plots,
#                          "tt_gif.gif",
#                          sep = "/")),
#   first_image_duration = 2,
#   last_image_duration = 10,
#   frame_duration = .2,
#   image_resize = 800
# )
# 
# gg_stop_recording()
### end of code ###