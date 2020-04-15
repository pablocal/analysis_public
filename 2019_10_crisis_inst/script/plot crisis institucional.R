# Metadata ----------------------------------------------------------------
# Title: Plot CIS institutional crisis
# Purpose:
# Author(s): @pablocal
# Date Created: 2019-10-30
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------

rm(list = ls())
library(tidyverse)
library(showtext)

font_add_google("Roboto Condensed", "RobotoC")
font_add_google("Roboto", "Roboto")
showtext_auto()

# 1. Data -----------------------------------------------------------------

val <- read_csv("2019_10_CrisisInst/data/valora_cis.csv") 
val$date <- parse_date(val$date, format = "%d/%m/%Y")
val <- val %>% 
  mutate(pol_neg = pol_mm + pol_m)

paro <- read_csv("2019_10_CrisisInst/data/paro.csv")
paro$date <- parse_date(paro$date, format = "%d/%m/%Y")


# Data wrangling ----------------------------------------------------------

# create a long file with negative evaluations
val_long <- val %>% 
  mutate(eco_neg = eco_m + eco_mm,
         pol_neg = pol_m + pol_mm) %>% 
  select(date, eco_neg, pol_neg) %>% 
  gather("value", "per", eco_neg:pol_neg) %>% 
  mutate(col = recode(value,
                      eco_neg = "burlywood3",
                      pol_neg = "coral3"),
         size = ifelse(value == "eco_neg", .5, 1)) %>% 
  filter(date > parse_date("01/01/2005", format = "%d/%m/%Y")) 

# paro with only the months after 2005
paro <- paro %>% 
  select(date, per_paro) %>%
  filter(date > parse_date("01/01/2005", format = "%d/%m/%Y"))

# rectangles
party <- tibble(party = c("PSOE", "PP", "PSOE"),
                xmin = parse_date(c("01/01/2005", "01/12/2011", "01/06/2018"), format = "%d/%m/%Y"),
                xmax = parse_date(c("01/11/2011", "01/05/2018", "01/10/2019"), format = "%d/%m/%Y"),
                ymin = replicate(3, 104),
                ymax = replicate(3, 110))

# annotations
annotations <- tibble(label = c("Situación\neconómica\nmala o muy mala", "Situación\npolítica\nmala o muy mala", 
                                "Tasa de\ndesempleo", "Generales\n2011", "Generales\n2008",
                                "Europeas\n2014", "Generales\n2015", "Generales\n2019"),
                      x = parse_date(c("15/10/2007", "01/04/2005", "01/01/2007", "01/06/2011", 
                                       "01/10/2007", "01/01/2014", "01/07/2015", "01/11/2018"), format = "%d/%m/%Y"),
                      col = c("burlywood3", "coral3", "darkgoldenrod3", "gray50", "gray50", "gray50", "gray50", "gray50"),
                      y = c(63, 40, 16, 97, 97, 97, 97, 97)
)

# arrows to fit some annotations (need two bc curvature cannot be in aesthetics)
curves <- tibble(x = parse_date(c("01/11/2007", "01/03/2005"), format = "%d/%m/%Y"), 
                 y = c(58, 35), 
                 xend = parse_date(c("15/04/2008", "01/08/2005"), format = "%d/%m/%Y"), 
                 yend = c(53, 30))

curves2 <- tibble(x = parse_date( "01/06/2007", format = "%d/%m/%Y"), 
                 y = 16, 
                 xend = parse_date("01/09/2007", format = "%d/%m/%Y"), 
                 yend = 9)


# segments for historical events
segments <-  tibble(x = parse_date(c("01/06/2014", "20/12/2015", "28/04/2019", "20/11/2011", "12/03/2008"), format = "%d/%m/%Y"), 
                    y = 0, 
                    xend = parse_date(c("01/06/2014", "20/12/2015", "28/04/2019", "20/11/2011", "12/03/2008"), format = "%d/%m/%Y"), 
                    yend = 101, 
                    col = "gray70")

# Plot --------------------------------------------------------------------

ggplot() +
  geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend, col = col), size = .2) +
  geom_curve(data = curves, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.07, "inches")), size = .3, col = "gray40") +
  geom_curve(data = curves2, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.07, "inches")), size = .3, curvature = -.3, col = "gray40") +
  geom_line(data = val_long, aes(x = date, y = per, group = value, col = col, size = size)) +
  geom_col(data = paro, aes(x = date, y = per_paro), fill = "darkgoldenrod3", alpha = .7) +
  geom_rect(data = party, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = party), alpha = .9) +
  geom_text(data = party, aes(x = xmin+250, y = ymin+3, label = party), col = "white", size = 3.5, fontface = "bold", family = "RobotoC") +
  geom_text(data = annotations, aes(x = x, y = y, col = col, label = label), size = 2.5, fontface = "bold", lineheight = .9, family = "RobotoC") +
  scale_y_continuous(limits = c(0, 111),
                     breaks = seq(0, 100, 25)) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_color_identity() +
  scale_size_identity() +
  scale_fill_manual(values = c(PP = "skyblue1", PSOE = "firebrick1")) +
  labs(title = "Cronificación de la crisis institucional en España",
       subtitle = "Valoración de la situación política, económica y tasa de desempleo",
       caption = "Elaboración @pablocalv • Datos CIS e INE") +
  theme(legend.position = "none",
        plot.caption = element_text(color = "gray50", family = "RobotoC"),
        plot.title = element_text(family = "Roboto", face = "bold"),
        plot.subtitle = element_text(family = "RobotoC"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "gray50", family = "RobotoC"),
        axis.text.x = element_text(angle = 30),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray90"))

ggsave("2019_10_CrisisInst/plot_crisis_inst.pdf", width = 9, height = 5.4)
ggsave("2019_10_CrisisInst/plot_crisis_inst.png", width = 9, height = 5.4)
