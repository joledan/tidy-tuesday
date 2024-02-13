# title: tt-2024-02-13
# date: february 13, 2024
# by: jan oledan
# desc: tidy tuesday challenge - valentines day consumer spending

##### install and load packages #####
rm(list=ls())

# package names
packages <- c("tidyverse", "sysfonts", "extrafont", "readxl", 
              "patchwork", "camcorder", "janitor", "tidytuesdayR",
              "ggtext")

# install packages not yet installed
install_package <- packages %in% rownames(installed.packages())
if (any(install_package == FALSE)) {
  install.packages(packages[!install_package])
}
invisible(lapply(packages, library, character.only = TRUE))

##### set up directories ######
main <- "tidy-tuesday/2024/2024-02-13"
plots <- "plots"

##### define main ggplot theme components #####
th <- theme_minimal(base_family = "Noto Sans") + 
  theme(legend.position = "none") +
  theme(panel.border = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.title = element_text(face = "bold", # plot title, subtitle, caption
                                  size = 12,
                                  margin = margin(t=0, r=0, b=2, l=0, "pt")),
        plot.subtitle = element_text(size = 8,
                                     margin = margin(t=0,r=0,b=4,l=0, "pt")),
        plot.caption = element_markdown(hjust = 0,
                                        size = 6,
                                        color = "#999999",
                                        margin = margin(t=4,r=0,b=0,l=0, "pt")),
        plot.caption.position = "plot") +
  theme(axis.title = element_blank(),
        axis.title.x = element_blank(), # adjust x axis title
        axis.title.y = element_blank(), # adjust y axis title
        axis.text.x = element_text(size = 8, # make axis text (labels) size 8
                                   colour = "#000000"),
        axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = -.5,
                                   hjust = 0),
        axis.ticks.length.x = unit(4, "pt"), # define tick length and colour for x-axis
        axis.ticks.x = element_line(colour="#000000"),
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.line.x = element_line(colour = "#000000"), # adjust x axis line
        panel.grid.major.x = element_blank(), # remove major x lines
        panel.grid.minor.x = element_blank(), # remove minor x lines
        panel.grid.minor.y = element_blank(), # remove minor y lines
        axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = -11, unit = "pt"))) # adjust tick length to go on top of line

### intialize camcorder to set up and output square plot ###
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 1,
  width = 3.5,
  height = 3.5,
  units = "in",
  dpi = 300,
  bg = "white"
)



##### read data #####
tuesdata <- tidytuesdayR::tt_load('2024-02-13')

historical_spending <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender


##### plot 1 - comparing national poverty rates #####

# re-organize levels
levels = c("Candy", "Flowers", "Jewelry", 
           "Greeting cards", "Evening out", "Clothing", "Gift cards")
# make data long 
gifts_gender_wide <- gifts_gender %>%
  pivot_longer(
    !Gender,
    names_to = "category",
    values_to = "percent"
  ) %>%
  mutate(category = case_when(
    category == "EveningOut" ~ "Evening out",
    category == "GreetingCards" ~ "Greeting cards",
    category == "GiftCards" ~ "Gift cards",
    T ~ category
  )) %>%
  filter(category != "SpendingCelebrating") %>%
  mutate(category = factor(category, levels = rev(levels)))

# trick to factor based on value of mean/diff/max - didn't end up using this
  # group_by(category) %>%
  # mutate(mean = mean(percent),
  #        diff = abs(diff(percent)),
  #        max = max(percent)) %>%
  # ungroup() %>%
  # arrange(-mean) %>%  # factor vars, sort by value, then re-factor the category
  # mutate(category = factor(category, levels = rev(levels)))

##### plot here #####

# define colours, based on Telegraph article on UK Gender Pay Gap
colours <- c("Women" = "#8624F5",
             "Men"="#1FC3AA")

f1 <- ggplot(data = gifts_gender_wide, 
             aes(y = category,
                 x = percent)) +
  th +
  geom_line(col = 'grey60',
            linewidth = 1) + 
  geom_point(aes(colour = Gender),
             size = 2) +
  scale_x_continuous(position = "top",
                     expand = c(0, 0),
                     limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  scale_color_manual(values = colours) +
  theme(axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = .5,
                                   hjust = 0)) +
  coord_cartesian(clip = "off") + # prevent points getting cut off
  theme(panel.spacing = unit(0,'pt'),
        plot.subtitle = element_markdown(lineheight = 1.2), # increase spacing between sentences
        plot.caption = element_markdown(lineheight = 1.2),
        plot.title.position = "plot",
        panel.grid.major.x = element_line(colour = "grey90"), # remove major x lines
        axis.line.x = element_line(colour = "grey90"),
        axis.ticks.x = element_line(colour="grey90"),
        axis.ticks.length.x = unit(0, "pt"), # define tick length and colour for x-axis
        axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid')) +
  theme() +
  labs(title = "Sweet, scented, and shiny",
       subtitle = "<span style='color:#8624F5'><b>Women</b></span> and <span style='color: #1FC3AA'><b>Men</b></span>
  spending money on Valentine's Day <br> gifts*, 2022 %",
       caption = "\\*Share of survey respondents spending money on gifts for Valentine's Day 2022. <br> **Source:** National Retail Foundation • **Visual:** Jan Oledan ")

# output plot
f1

# make gif!
gg_playback(
  name = file.path(paste(plots,
                         "tt_gif.gif",
                         sep = "/")),
  first_image_duration = 2,
  last_image_duration = 10,
  frame_duration = .2,
  image_resize = 800
)

gg_stop_recording()
### end of code ###