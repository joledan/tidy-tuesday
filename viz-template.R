# title: viz-template
# date: 
# by: jan oledan
# desc: ggplot/visual template 

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
main <- ""
plots <- ""

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