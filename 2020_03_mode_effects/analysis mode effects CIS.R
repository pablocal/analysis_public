# Metadata ----------------------------------------------------------------
# Title: Mode effects CIS april 2020
# Objective:
# Author(s): @pablocal
# Date: 2020-04-15
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------
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
                     axis.text.x = element_text(color = "gray20", family = "Roboto_c"),
                     axis.title.x = element_text(color = "black", face = "bold", family = "Roboto_c"),
                     axis.text.y = element_text(color = "gray20", family = "Roboto_c"),
                     plot.background = element_rect(fill = "white"),
                     panel.background = element_rect(fill = "white"),
                     strip.background = element_rect(fill = "black"),
                     strip.text = element_text(color = "white", family = "Roboto_c"))
# 1. Data -----------------------------------------------------------------
d <- haven::read_sav("2020_03_mode_effects/data/3271.sav")
d <- d %>% 
  mutate(EDAD = as.integer(EDAD)) %>% 
  haven::as_factor() %>% 
  rename_all(str_to_lower)

# 2. Select sociodemographics ---------------------------------------------
colnames(d)

rv_others <-  c("En Comú Podem", "Més Compromís", "ERC", "JxCat", 
                "EAJ-PNV", "EH Bildu", "CCa-NC", "Na+", "CUP", 
                "BNG", "PRC", "Más País", "En Común-Unidas Podemos", 
                "Teruel Existe", "Nulo", "No tenía derecho a voto", 
                "No tenía edad", "Otros partidos", "En blanco")

d <- d %>% 
  select(sexo, edad, tamuni, escideol, recuerdo, estudios, 
         religion, estadocivil, sitlab, clasesub, cno11) %>%
  mutate(edad_grp = case_when(
    between(edad, 18, 24) ~ "De 18 a 24 años",
    between(edad, 25, 34) ~ "De 25 a 34 años",
    between(edad, 35, 44) ~ "De 35 a 44 años",
    between(edad, 45, 54) ~ "De 45 a 54 años",
    between(edad, 55, 64) ~ "De 55 a 64 años",
    between(edad, 65, Inf) ~ "65 o más años"),
    ideo_grp = fct_collapse(escideol,
                           "Izq. (1-4)" = c("1 Izquierda", "2", "3", "4"),
                           "Centro (5-6)" =  c("5", "6"),
                           "Der. (6-10)" = c("7", "8", "9", "10 Derecha")),
    recuerdo = fct_collapse(recuerdo, "N.S." = rv_others)
    ) 


# 3. Export table ---------------------------------------------------------
export_table <- d %>% 
  select(sexo, edad_grp, tamuni, everything(), -edad, -escideol) %>% 
  sjmisc::frq() %>% 
  reduce(bind_rows) %>% 
  filter(!is.na(val)) %>%
  filter(!(val %in% c("N.S.", "N.C.", "No recuerda", "Otras", "Otros", "Otra/o"))) %>% 
  mutate(jan = raw.prc,
         feb = 0,
         mar = 0,
         apr = 0) %>% 
  select(val, jan:apr)

# write_csv(export_table, "data/export.csv")


# 4. get data with feb-apr ------------------------------------------------
d <- read_csv("2020_03_mode_effects/data/export.csv") %>% 
  mutate(strata = ifelse(var %in% c("Sexo", "Edad", "Tamaño de municipio"), "Variables estratos/cuotas", "Otras variables"),
         strata = fct_relevel(strata, "Variables estratos/cuotas"))


# Plot average dev of vars ------------------------------------------------
d_avg_bias <- d %>% 
  mutate(jan_feb = abs(jan-feb),
         feb_mar = abs(feb-mar),
         mar_apr = abs(mar-apr)) %>% 
  group_by(strata, var, card, wording_change) %>% 
  summarise(jan_feb = sum(jan_feb)/n(),
            feb_mar = sum(feb_mar)/n(),
            mar_apr = sum(mar_apr)/n()) %>% 
  ungroup() %>% 
  gather("month", "avg_bias", jan_feb:mar_apr) %>% 
  mutate(month = fct_relevel(month, "jan_feb", "feb_mar"),
         month_label = recode(month, "jan_feb" = 'Ene. ~ Feb.',
                              "feb_mar" = "Feb.~ Mar.",
                              "mar_apr" = "Mar. ~ Abr." ),
         col = ifelse(var %in% c("Estudios", "Escala ideológica", "Clase (subjetiva)"), 1, 0)) %>% 
  group_by(strata) %>% 
  mutate(rank = as_factor(rank(var))) %>% 
  ungroup()

rect <- tibble(xmin = c(0, 2.5, 3.51),
               xmax = c(2.49, 3.5, 4.2),
               ymin = c(6.75, 6.75, 6.75),
               ymax = c(7.25, 7.25, 7.25),
               fill = c("gray5", "gray25", "white"),
               alpha = .3)


ggplot() + 
  geom_point(data = d_avg_bias, aes(x = month_label, y = avg_bias, group = var, col = rank), alpha = .7) +
  geom_line(data = d_avg_bias, aes(x = month_label, y = avg_bias, group = var, col = rank)) +
  geom_text(data = filter(d_avg_bias, month == "mar_apr"), mapping = aes(x = 3.1, y = avg_bias, label = var, col = rank), position = position_jitter(height=.1, width = 0), size = 2.5, hjust = 0, family = "Roboto_c") +
  geom_rect(data = rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, alpha = alpha), col = "white") +
  annotate(geom = "text", label = "Presencial", y = 7, x = 1.5, size = 3, fontface = "bold", family = "Roboto_c", col = "white") +
  annotate(geom = "text", label = "Pres. ~ Telef.", y = 7, x = 3, size = 3, fontface = "bold", family = "Roboto_c", col = "white") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_identity() +
   labs(title = "Variación de un mes a otro de las variables de clasificación",
       subtitle = "Barómetros CIS Ene.-Abr.",
       x = "",
       y = "Variación media (%)",
       caption = "@pablocalv con datos CIS") +
  facet_wrap( ~ strata) +
  theme_reg

ggsave("2020_03_mode_effects/plot_variables.pdf")

# Plot changes in wording or card -----------------------------------------
d_card <- d_avg_bias %>% 
  filter(month == "mar_apr") %>% 
  mutate(card_wording = "Uso de tarjeta en modo presencial")

d_card_wording <- d_avg_bias %>% 
  filter(month == "mar_apr") %>% 
  mutate(card = wording_change,
         card_wording = "Cambio de la pregunta") %>%
  bind_rows(d_card) %>% 
  mutate(card = recode(card, `1` = "Sí", `0` = "No"),
         card = fct_relevel(card, "Sí"))

ggplot(d_card_wording, aes(x = factor(card), y = avg_bias, fill = card)) +
  geom_boxplot(alpha= .4) +
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "red") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap( ~ card_wording) +
  labs(title = "Algunos cabios acentúan las diferencias entre modos de adminsitración", 
       subtitle = "Barómetros CIS Marzo-Abril",
       x = "",
       y = "Variación media (%)",
       caption = "@pablocalv con datos CIS") + 
  theme_reg 

ggsave("2020_03_mode_effects/plot_changes.pdf")

# Plot three variables ----------------------------------------------------
d_vars <- d %>% 
  filter(var %in% c("Estudios", "Escala ideológica", "Clase (subjetiva)")) %>% 
  gather("month", "per", jan:apr) %>% 
  mutate(month = recode(month, "jan" = "Ene.",
                        "feb" = "Feb.",
                        "mar" = "Mar.",
                        "apr" = "Abr."),
         month = fct_relevel(month, "Ene.", "Feb.", "Mar.")) %>% 
  group_by(var, month) %>% 
  mutate(y = min(per),
         yend = max(per)) %>% 
  ungroup() %>% 
  group_by(var) %>% 
  mutate(rank = as_factor(rank(val))) %>% 
  ungroup()

rect <- tibble(xmin = c(0, 3.5, 4.51),
               xmax = c(3.49, 4.5, 5.9),
               ymin = c(47, 47, 47),
               ymax = c(50, 50, 47),
               fill = c("gray5", "gray25", "white"),
               alpha = .3)

ggplot() +
  geom_segment(data = d_vars, aes(x = month, xend = month, y = y, yend = yend), col = "gray70", size = .03) +
  geom_line(data = d_vars, aes(x = month, y = per, group = val, col = rank)) +
  geom_point(data = d_vars, aes(x = month, y = per, group = val, col = rank)) +
  geom_text(data = filter(d_vars, month == "Abr."), mapping = aes(x = 4.1, y = per, label = val, col = rank), size = 2.5, hjust = 0, family = "Roboto_c") +
  geom_rect(data = rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, alpha = alpha), col = "white") +
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_identity() +
  facet_wrap( ~ var) +
  annotate(geom = "text", label = "Presencial", y = 48.5, x = 1, size = 3, fontface = "bold", family = "Roboto_c", col = "white") +
  annotate(geom = "text", label = "Telef.", y = 48.5, x = 4, size = 3, fontface = "bold", family = "Roboto_c", col = "white") +
  labs(title = "Cambio del modo de administración del cuestionario", 
       subtitle = "Barómetros CIS Enero-Abril",
       x = "",
       y = "",
       caption = "@pablocalv con datos CIS") + 
  theme_reg

ggsave("2020_03_mode_effects/plot_vars.pdf")
