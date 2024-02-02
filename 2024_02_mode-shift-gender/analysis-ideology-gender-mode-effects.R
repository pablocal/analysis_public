# R-script - Metadata -----------------------------------------------------
# Title: analysis-ideology-gender-mode-effects.R
# Purpose:  Analyse change of modes and ideology shift (2020-24)
# Author(s): @pablocal
# Date: 2024-02-02
#
# Comments ----------------------------------------------------------------
# 
#
#
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)
library(tidyverse)
library(showtext)
library(ggrepel)

font_add_google("Roboto Condensed", "Roboto_c")
font_add_google("Roboto", "Roboto")

showtext_auto()

theme_reg <- theme(plot.title = element_text(family = "Roboto_c", hjust = -.01, face = "bold"),
                     plot.subtitle = element_text(family = "Roboto_c", hjust = -.01),
                     plot.caption = element_text(family = "Roboto_c", color = "grey40", size = 8),
                     legend.position = "none",
                     axis.line.x = element_line(color = "gray20"), 
                     axis.title.y = element_text(color = "black", face = "bold", family = "Roboto_c"),
                     axis.ticks.y = element_blank(),
                     axis.text.x = element_text(color = "gray20", family = "Roboto_c", angle = 75, hjust = 1),
                     axis.title.x = element_text(color = "black", face = "bold", family = "Roboto_c"),
                     axis.text.y = element_text(color = "gray20", family = "Roboto_c"),
                     plot.background = element_rect(fill = "white"),
                     panel.background = element_rect(fill = "white"),
                     strip.background = element_rect(fill = "black"),
                     strip.text = element_text(color = "white", family = "Roboto_c"))
# 1. Data -----------------------------------------------------------------
d <- haven::read_sav("2024_02_mode-shift-gender/2_data/FID_3620.sav") %>% 
  select(ESTUDIO, AÑOMES, AÑO, MES, NUMENTR, A.3, A.4, C.3.1, H.1) %>% 
  rename(idstu = ESTUDIO,
         yrmn = AÑOMES,
         yr = AÑO,
         mn = MES,
         id = NUMENTR,
         sex = A.3,
         age = A.4,
         ideo = C.3.1,
         wt = H.1) %>% 
  mutate(age = as.integer(age),
         mn = as.integer(mn)) %>% 
  haven::as_factor()
glimpse(d)


# 2. Select sociodemographics ---------------------------------------------
d <- d %>% 
  mutate(ageg = case_when(
                    between(age, 18, 24) ~ "De 18 a 24 años",
                    between(age, 25, 34) ~ "De 25 a 34 años",
                    between(age, 35, 44) ~ "De 35 a 44 años",
                    between(age, 45, 54) ~ "De 45 a 54 años",
                    between(age, 55, 64) ~ "De 55 a 64 años",
                    between(age, 65, Inf) ~ "65 o más años"),
          ideog = fct_collapse(ideo,
                                 "Izq. (1-4)" = c("1 Izquierda", "2", "3", "4"),
                                 "Centro (5-6)" =  c("5", "6"),
                                 "Der. (6-10)" = c("7", "8", "9", "10 Derecha")),
         ideo = as.integer(ideo),
         quarter = case_when(
                  between(mn, 1, 3) ~ "Q1",
                  between(mn, 4, 6) ~ "Q2",
                  between(mn, 7, 9) ~ "Q3",
                  between(mn, 10, 12) ~ "Q4"),
         yq = paste0(yr, quarter),
         wt = ifelse(is.na(wt),1 , wt)
          ) 

d$ageg <- factor(d$ageg, levels = c("De 18 a 24 años", 
                                    "De 25 a 34 años",
                                    "De 35 a 44 años",
                                    "De 45 a 54 años",
                                    "De 55 a 64 años",
                                    "65 o más años")) 

# 3. Calculations ---------------------------------------------------------

avg <- d %>%
  filter(sex != "N.C") %>% 
  group_by(yq, ageg, sex) %>% 
  summarise(avg_ideo = weighted.mean(ideo, wt, na.rm = T),
            se_avg_ideo = sqrt(sum(wt * (ideo - weighted.mean(ideo, wt, na.rm = TRUE))^2, na.rm = T) /
                                      (sum(wt)^2 - sum(wt^2) / sum(wt))), 
            n = n()) %>% 
  ungroup() %>% 
  mutate(lb = avg_ideo - 1.96*se_avg_ideo,
         ub = avg_ideo + 1.96*se_avg_ideo)
  


# Plot trend over time ------------------------------------------------
display_labels <- d$yq %>% 
  unique() %>% 
  .[c(TRUE, FALSE)]

lbls <- tibble(ageg = "De 18 a 24 años",
               x = c(1, 1),
               y = c(5.25, 5.05),
               sex = c("Hombre", "Mujer")
              )
lbls$ageg <- factor(lbls$ageg, levels = c("De 18 a 24 años", 
                                    "De 25 a 34 años",
                                    "De 35 a 44 años",
                                    "De 45 a 54 años",
                                    "De 55 a 64 años",
                                    "65 o más años"))                      

ggplot(avg, aes(x = yq, y = avg_ideo, col = sex)) + 
  geom_pointrange(aes(ymin = lb, ymax = ub)) +
  geom_point(alpha = .7) +
  geom_line(aes(group = sex)) +
  geom_text(data = lbls, mapping = aes(x = x, y = y, label = sex, col = sex), size = 5, hjust = 0, family = "Roboto_c", fontface = "bold") +
  scale_y_continuous(breaks = seq(3.5, 5.5, .5)) +
  scale_x_discrete(breaks = display_labels) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_identity() +
  labs(title = "Evolución de la media de autoubicación ideológica por género",
       subtitle = "De enero 2018 a diciembre 2023",
       x = "",
       y = "Autoubicación ideológica (media)",
       caption = "@pablocalv con datos CIS") +
  facet_wrap( ~ ageg) +
  theme_reg
ggsave("2024_02_mode-shift-gender/gender-ideology-age.pdf")
ggsave("2024_02_mode-shift-gender/gender-ideology-age.png")

avg %>% 
  filter(ageg == "De 18 a 24 años") %>% 
  ggplot(aes(x = yq, y = avg_ideo, col = sex)) + 
  geom_pointrange(aes(ymin = lb, ymax = ub)) +
  geom_point(alpha = .7) +
  geom_line(aes(group = sex)) +
  geom_text(data = lbls, mapping = aes(x = x, y = y, label = sex, col = sex), size = 5, hjust = 0, family = "Roboto_c", fontface = "bold") +
  geom_rect(xmin = 1, xmax = 7.9, ymin = 5.7, ymax = 5.9, fill = "gray60", alpha = .3, col = "white") +
  geom_rect(xmin = 8, xmax = 25, ymin = 5.7, ymax = 5.9, fill = "gray45", alpha = .3, col = "white") +
  annotate(geom = "text", label = "Presencial (CAPI)", y = 5.80, x = 3, size = 4, fontface = "bold", family = "Roboto_c", col = "white") +
  annotate(geom = "text", label = "Telefónico (CATI)", y = 5.8, x = 10, size = 4, fontface = "bold", family = "Roboto_c", col = "white") +
  scale_y_continuous(breaks = seq(3.5, 5.5, .5), limits = c(3.5, 5.9)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_identity() +
  labs(title = "Evolución de la media de autoubicación ideológica por género",
       subtitle = "Personas entre 18 y 24 años de edad",
       x = "",
       y = "Autoubicación ideológica (media)",
       caption = "@pablocalv con datos CIS") +
  theme_reg

ggsave("2024_02_mode-shift-gender/gender-ideology-1824.pdf")
ggsave("2024_02_mode-shift-gender/gender-ideology-1824.png")

