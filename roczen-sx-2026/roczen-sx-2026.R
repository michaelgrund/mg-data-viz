library(tidyverse)
library(ggbump)
library(ggflags)
library(magick)
library(showtext)
library(sysfonts)
library(ggtext)
library(glue)

# if not already installed, just search for the required fonts online
font_add_google(name = "Bebas Neue", family = "bebas")
font_add_google(name = "Barlow Condensed", family = "barlow", )
font_add(family = "Font Awesome 7 Brands", regular = "fonts/otfs/Font Awesome 7 Brands-Regular-400.otf")
font_add(family = "fa-solid", regular = "fonts/otfs/Font Awesome 7 Free-Solid-900.otf")

showtext_auto()

width <- 10
height <- 5

color_ktm <- "#f56900"
color_suzuki <- "#ffe217"
color_yamaha <- "#4169e1"
color_honda <- "#cc0000"
color_kawa <- "#6bbf23"
color_husky <- "#13408e"

df_colors_brands <- tibble(
  brand = c("KTM", "Suzuki", "Yamaha", "Honda", "Kawasaki", "Husqvarna"),
  color = c(color_ktm, color_suzuki, color_yamaha, color_honda, color_kawa, color_husky)
)

title <- "Ken Roczen's ride to the 2026 AMA SX 450 title"

github_icon <- "&#xf09b"
github_name <- "michaelgrund"
social_caption <- glue("<span style='font-family:\"Font Awesome 7 Brands\";'>{github_icon};</span>
  <span style='color: #FFFFFF'>{github_name}</span>"
)

caption <- str_c("**Source**: supercrosslive.com **Visualization**: Michael Grund (", social_caption, ")")

# raw data extracted via Claude
df_standings <- tibble(
  pos = 1:10,
  no = c(94, 96, 2, 3, 32, 4, 27, 17, 26, 46),
  brand = c("Suzuki", "Honda", "Yamaha", "KTM", "Yamaha",
            "Kawasaki", "Husqvarna", "Honda", "KTM", "KTM"),
  rider = c("Ken Roczen", "Hunter Lawrence", "Cooper Webb",
            "Eli Tomac", "Justin Cooper", "Chase Sexton",
            "Malcolm Stewart", "Joey Savatgy", "Jorge Prado",
            "Justin Hill"),
  points_overall = c(349, 346, 315, 275, 273, 237, 204, 194, 189, 188),
  adjustments    = c(0, 0, 0, 0, 0, 0, 0, 0, -3, 0),
  `Anaheim 1`    = c(22, 18, 15, 25, 16, 14, NA,  9, 20,  8),
  `San Diego`    = c(20, 22, 14, 25, 16, 18, 12, 17,  9,  8),
  `Anaheim 2`    = c(14, 22, 17, 20, 12, 25, 10, 16, 15,  7),
  `Houston`      = c(20, 22, 25, 18, 13, 17, 14,  8, 15,  5),
  `Glendale`     = c(25, 22, 20, 10, 18, 15, NA, 16, 17, 12),
  `Seattle`      = c(12, 18, 22, 25, 20, 17, 16, 14, NA, 11),
  `Arlington`    = c(18, 25, 20, 22, 17, 16, 11, 15, NA, 10),
  `Daytona Beach`= c(20, 22, 18, 25, 10, NA, 12, 17, NA, 13),
  `Indianapolis` = c(17, 25, 20, 22, 18, NA, 15, 13, 16, 10),
  `Birmingham`   = c(22, 25, 16, 20, 18, NA, 17, NA, 15, 11),
  `Detroit`      = c(25,  4, 16, 17, 18, 22, 20, 13,  9, 14),
  `St. Louis`    = c(25, 20, 17, 16, 22, NA, 14, 15, 18,  8),
  `Nashville`    = c(20, 25, 22, 10, 15, 18,  4,  6,  9, 17),
  `Cleveland`    = c(25, 16, 22, NA, 20, 18, 15, 17,  7, 14),
  `Philadelphia` = c(25, 20, 22, NA,  9, 15, 12, 18,  6, 17),
  `Denver`       = c(22, 25, 11, 20,  9, 17, 18, NA, 16,  7),
  `Salt Lake City`= c(17, 15, 18, NA, 22, 25, 14, NA, 20, 16)
)

location_levels <- df_standings %>% 
  select(`Anaheim 1`:`Salt Lake City`) %>% 
  colnames()

df_standings <- df_standings %>%  
  # add country iso code for each rider, needed for plotting flags later
  mutate(country = c("de", "au", "us", "us", "us", "us", "us", "us", "es", "us")) %>% 
  pivot_longer(`Anaheim 1`:`Salt Lake City`, names_to = "location", values_to = "points") %>%
  left_join(df_colors_brands, join_by(brand)) %>%
  select(-points_overall, -adjustments, -pos) %>%
  group_by(rider) %>%
  mutate(cumsum = cumsum(replace_na(points, 0))) %>%
  ungroup() %>%
  mutate(location = factor(location, levels = location_levels)) %>%
  group_by(location) %>%
  arrange(-cumsum) %>%
  mutate(pos = row_number()) %>%
  ungroup() %>%
  arrange(location) %>%
  mutate(highlight = rider == "Ken Roczen" )

df_riders_final <- df_standings %>% 
  filter(location == "Salt Lake City") %>% 
  select(no, location, rider, pos, color, country) %>% 
  filter(pos <= 5)

top5_last <- df_riders_final %>% 
  pull(rider)

# only the individual event result positions of the final top 5 are shown in the plot for clarity
df_standings <- df_standings %>% 
  filter(rider %in% top5_last)

# general theme settings
theme_general <-  theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_text(color = "white", 
                             family = "barlow", 
                             size = 27.5, 
                             angle = 45, 
                             vjust = 1, 
                             hjust = 1),
  legend.position = "none",
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(color = "white", 
                            family = "bebas", 
                            size = 75, 
                            hjust = .5),
  plot.subtitle = element_text(color = "white", 
                               family = "barlow", 
                               hjust = .5, 
                               size = 10),
  plot.caption = element_markdown(color = "white", 
                                  family = "barlow", 
                                  hjust = 1, 
                                  size = 22),
  text = element_text(family = "barlow")
)

#-------------------------------------------------------------------------------
# generating a mask for showing the highlight line as dirt image

mask_plot <- df_standings %>% 
  filter(rider == "Ken Roczen") %>% 
  ggplot(aes(location, pos, color = color, group = rider)) +
  geom_bump(linewidth = 15, color = "black", lineend = "round") +
  scale_y_reverse(breaks = 1:5, labels = NULL) +       
  coord_cartesian(ylim = c(5, .9)) + 
  scale_color_identity() +
  ggtitle(title) +
  xlab("") +
  labs(caption = caption) +
  theme_general +
  theme(
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  ) + 
  scale_x_discrete(expand = expand_scale(mult = c(.075, .3)))


ggsave("mask.png", mask_plot, width = width, height = height)

#-------------------------------------------------------------------------------
# main plot without highlight line

# add slight white to differentiate Cooper and Webb
df_standings_cooper <- df_standings %>% 
  filter(rider == "Justin Cooper")

main_plot <- df_standings %>% 
  filter(rider != "Ken Roczen") %>% 
  ggplot(aes(location, pos, color = color, group = rider)) +
  geom_bump(linewidth = 1, show.legend = F) +
  geom_bump(data = df_standings_cooper, linewidth = 1, color = "white", alpha = .3, show.legend = F) +
  scale_y_reverse(breaks = 1:5, labels = NULL) +      
  coord_cartesian(ylim = c(5, .9)) + 
  scale_color_identity() +
  geom_point(size = 1.2, show.legend = F) +
  ggtitle(title) +
  xlab("") +
  labs(caption = caption) +
  theme_general +
  theme(panel.background = element_rect(fill = "gray20"),
        plot.background = element_rect(fill = "gray20"),
        # fine vertical lines to better indicate events
        panel.grid.major.x = element_line(color = "#ffffff20", linewidth = .2)
  ) +
  scale_x_discrete(expand = expand_scale(mult = c(.075, .3)))

ggsave("main.png", main_plot, width = width, height = height)

#-------------------------------------------------------------------------------
# highlight line

event_wins <- function(location_name, pos_overall){
  annotate(
    geom = "text",
    x = which(levels(df_standings$location) == location_name),
    y = pos_overall - .15,
    label = "\uf091", # Font Awesome trophy
    family = "fa-solid",
    size = 10,
    color = "white"
  )
}

df_pos_labels <- tibble(
  x     = .55,
  y     = 1:5,
  label = c(
    "<span style='color:#FFFFFF; font-size:35pt; font-family:barlow;'>1<sup>st<sup></span>",
    "<span style='color:#FFFFFF; font-size:35pt; font-family:barlow;'>2<sup>nd<sup></span>",
    "<span style='color:#FFFFFF; font-size:35pt; font-family:barlow;'>3<sup>rd<sup></span>",
    "<span style='color:#FFFFFF; font-size:35pt; font-family:barlow;'>4<sup>th<sup></span>",
    "<span style='color:#FFFFFF; font-size:35pt; font-family:barlow;'>5<sup>th<sup></span>"
  )
)

highlight_plot <- df_standings %>% 
  filter(rider == "Ken Roczen") %>% 
  ggplot(aes(location, pos, color = color, group = rider)) +
  scale_y_reverse(breaks = 1:5, labels = NULL) +       
  coord_cartesian(ylim = c(5, .9)) + 
  scale_color_identity() +
  geom_point(size = 2, show.legend = F) +
  # simulate lane
  geom_bump(linewidth = 6.5, color = "black", alpha = .075, lineend = "round") +
  geom_bump(linewidth = 5.5, color = "black", alpha = .075, lineend = "round") +
  geom_bump(linewidth = 4.5, color = "black", alpha = .075, lineend = "round") +
  geom_bump(linewidth = 4, color = "black", alpha = .075, lineend = "round") +
  geom_bump(linewidth = 3, color = "black", alpha = .075, lineend = "round") +
  geom_bump(linewidth = 1.25, color = color_suzuki) +
  geom_point(size = 2, show.legend = F) +
  geom_richtext(data = df_pos_labels, aes(x = x, y = y, label = label), fill = NA,
                label.color = NA, inherit.aes = F) +
  geom_text(data = df_riders_final, aes(location, label = str_c("#", no)),
            show.legend = F, hjust = .5, nudge_x = .6, 
            family = "barlow", size = 16) +
  # workaround to add circular edge to flags
  geom_point(inherit.aes = F, 
             data = df_riders_final, aes(x = as.numeric(location) + 1.25, y = pos),
             size = 7, color = "white") +
  geom_flag(data = df_riders_final, 
            aes(as.numeric(location) + 1.25, pos, country = country),
            size = 6) +
  geom_text(data = df_riders_final, aes(location, label = rider),
            show.legend = F, hjust = 0, nudge_x = 1.625, 
            family = "barlow", size = 16) +
  ggtitle(title) +
  xlab("") +
  labs(caption = caption) +
  theme_general +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA)) +
  scale_x_discrete(expand = expand_scale(mult = c(.075, .3))) +
  # trophy icons at Roczen's main event wins
  event_wins(location_name = "Glendale", pos_overall = 2) +
  event_wins(location_name = "Detroit", pos_overall = 3) +
  event_wins(location_name = "St. Louis", pos_overall = 3) +
  event_wins(location_name = "Cleveland", pos_overall = 2) +
  event_wins(location_name = "Philadelphia", pos_overall = 1) +
  # annotations
  annotate("curve", 
           x = which(levels(df_standings$location) =="Daytona Beach"), 
           y = 4.5, 
           xend = which(levels(df_standings$location) =="Arlington") + .05, 
           yend = 4.15,
           arrow = arrow(length = unit(.15, "cm"), type = "closed"),
           size = .5,
           linewidth = .4, 
           curvature = .3,
           color = color_suzuki) +
  annotate(x = which(levels(df_standings$location) == "Indianapolis") + .095 , y = 4.5, 
           label = "Overall position after Arlington",
           geom = "text", size = 10, color = color_suzuki,
           family = "barlow",
           hjust = .325) +
  annotate("curve", 
           x = which(levels(df_standings$location) =="Seattle"), 
           y = 1.3, 
           xend = which(levels(df_standings$location) =="Glendale") + .1, 
           yend = 1.75,
           arrow = arrow(length = unit(.15, "cm"), type = "closed"),
           size = .5,
           linewidth = .4, 
           curvature = -.3,
           color = "white") +
  annotate(x = which(levels(df_standings$location) =="Seattle") + 1.1 , y = 1.3, 
           label = "Winner Glendale 450 main event",
           geom = "text", size = 10, color = "white" ,
           family = "barlow",
           hjust = .325) 


ggsave("highlight.png", highlight_plot, width = width, height = height, bg = "transparent")

#-------------------------------------------------------------------------------
# build together final figure

texture <- image_read("data/dirt_mod.png") %>% 
  image_resize("3000x3000!") %>% 
  # colorize dirt in yellow
  image_colorize(opacity = 25, color = color_suzuki)

mask <- image_read("mask.png") %>% 
  image_convert(colorspace = "gray") %>% 
  image_negate()

textured_line <- image_composite(texture, mask, operator = "CopyOpacity") %>% 
  image_fx("a*0.85", channel = "alpha")

textured_line %>%   
  image_write("textured_line.png")

main <- image_read("main.png")
highlight <- image_read("highlight.png")

main %>% 
  image_composite(textured_line, operator = "Over") %>% 
  image_composite(highlight, operator = "Over") %>% 
  image_write("fig_sx_roczen_2026.png")