options(scipen=30, digits=2)

# pacotes -------------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)

# carregando a base ---------------------------------------------------------------------------

df <- readxl::read_excel(path = "echo_scores.xlsx") |>
  mutate(group = if_else(group == "Ex", true = "Exercised group",false = "Non-exercise group"))

head(df,n = 12)

# funçao para plot repetido -------------------------------------------------------------------

figura <- function(base, variavel){
  base |>
    ggplot(aes(x = forcats::fct_reorder(tempo, desc(tempo)) , y = variavel, group = id, color = group)) +
    geom_point() +
    geom_line() +
    facet_grid(~group) +
    theme_classic() +
    scale_colour_manual(values = c("#04cae0", "#e004d1")) +
    labs(x= "") +
    theme(legend.position = "top",
          legend.title = element_blank())
}

# figuras -------------------------------------------------------------------------------------
#BSA
bsa <-
  figura(df, df$BSA) +
  labs(y = bquote("Body Surface Area (z-socre)"))

#LVEDd
LVEDd <-
  figura(df, df$LVEDd_z) +
  labs(y = bquote("LVEDd (z-socre)"))

#LVESd_z"
LVESd <-
  figura(df, df$LVESd_z) +
  labs(y = bquote("LVESd (z-socre)"))

# "E/e_z"
E_e <-
  figura(df, df$`E/e_z`) +
  labs(y = bquote("E/e (z-socre)"))

# "E/A_z"
E_A <-
  figura(df, df$`E/A_z`) +
  labs(y = bquote("E/A (z-socre)"))

# "TAPSE_z"
TAPSE <-
  figura(df, df$TAPSE_z) +
  labs(y = bquote("TAPSE(z-socre)"))

# "OMNI_LV_mass_calculator"
OMNI_LV_mass_calculator <-
  figura(df, df$OMNI_LV_mass_calculator) +
  labs(y = bquote("OMNI LV mass calculator (z-socre)"))

# "LVEF%"
LVEF <-
  figura(df, df$`LVEF%`) +
  labs(y = bquote("LVEF% (z-socre)"))

# "GLS%"
GLS <-
  figura(df, df$`GLS%`) +
  labs(y = bquote("GLS% (z-socre)"))

# layout --------------------------------------------------------------------------------------

(bsa | LVEDd | LVESd) / (E_e | E_A | TAPSE) / (OMNI_LV_mass_calculator|LVEF|GLS)












