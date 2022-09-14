# Pacotes
library(tidyverse)
library(fmsb)

# Ajustando a base para fazer o grafico -------------------------------------------------------
# Lendo a base
df <- readxl::read_xlsx("questionario.xlsx")

# Definindo a amplitude do escore: Minimo e máximo
max_min <- tibble(
  "Upper extremity" = c(100, 0),
  "Transfer and basic mobility" = c(100, 0),
  "Sports and physical" = c(100, 0),
  "Pain/comfort" = c(100, 0),
  Happiness = c(100, 0),
  "PODCI global" = c(100, 0)
) |>
  as.data.frame()

row.names(max_min) <- c("Max", "Min") # inserindo o nome das linhas

max_min

# Patient 1 -----------------------------------------------------------------------------------

# Sumarizando as variaveis que serãp plotadas
score_1 <- df |>
  filter(ID == 1,
         questionario == "podci") |>
  select(
    -ID,
    -time,
    -questionario,
    -Physical,
    -Emotional,
    -Social,
    -School,
    -Psychosocial,
    -"Global PedsQL") |>
  as.data.frame()

row.names(score_1) <- c("PRE", "POST") # inserindo o nome das linhas

score_1

# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar_1 <- rbind(max_min, score_1)

df_radar_1

# Plot the data for student 1
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar_1,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar_1),
           axislabcol = "grey",
           vlcex = 2,
           pcol = c("#04cae0", "#e004d1"), # Customizando o poligono
           pfcol = scales::alpha(c("#04cae0", "#e004d1"), 0.1),
           plwd = 2,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1
           # title = "Patient I"
)

# Patient 2 -----------------------------------------------------------------------------------

# Sumarizando as variaveis que serãp plotadas
score_2 <- df |>
  filter(ID == 2,
         questionario == "podci") |>
  select(
    -ID,
    -time,
    -questionario,
    -Physical,
    -Emotional,
    -Social,
    -School,
    -Psychosocial,
    -"Global PedsQL") |>
  as.data.frame()

row.names(score_2) <- c("PRE", "POST") # inserindo o nome das linhas

score_2


# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar_2 <- rbind(max_min, score_2)

df_radar_2

# Plot the data for student 2
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar_2,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar_2),
           axislabcol = "grey",
           vlcex = 2,
           pcol = c("#04cae0", "#e004d1"), # Customizando o poligono
           pfcol = scales::alpha(c("#04cae0", "#e004d1"), 0.1),
           plwd = 2,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1
           # title = "Patient II"
)


# Patient 3 -----------------------------------------------------------------------------------

# Sumarizando as variaveis que serãp plotadas
score_3 <- df |>
  filter(ID == 3,
         questionario == "podci") |>
  select(
    -ID,
    -time,
    -questionario,
    -Physical,
    -Emotional,
    -Social,
    -School,
    -Psychosocial,
    -"Global PedsQL")  |>
  as.data.frame()

row.names(score_3) <- c("PRE", "POST") # inserindo o nome das linhas

score_3


# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar_3 <- rbind(max_min, score_3)

df_radar_3

# Plot the data for student 2
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar_3,
            axistype = 1, # Cutomizando as labels
            caxislabels = c(0, 25, 50, 75, 100),
            vlabels = colnames(df_radar_3),
            axislabcol = "grey",
            vlcex = 2,
            pcol = c("#04cae0", "#e004d1"), # Customizando o poligono
            pfcol = scales::alpha(c("#04cae0", "#e004d1"), 0.1),
            plwd = 2,
            plty = 2,
            cglcol = "grey", # Customizando a grid
            cglwd = 0.8,
            cglty = 1
            # title = "Patient III"
)

# Patient 4 -----------------------------------------------------------------------------------

# Sumarizando as variaveis que serãp plotadas
score_4 <- df |>
  filter(ID == 4,
         questionario == "podci") |>
  select(
    -ID,
    -time,
    -questionario,
    -Physical,
    -Emotional,
    -Social,
    -School,
    -Psychosocial,
    -"Global PedsQL") |>
  as.data.frame()

row.names(score_4) <- c("PRE", "POST") # inserindo o nome das linhas

score_4


# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar_4 <- rbind(max_min, score_4)

df_radar_4

# Plot the data for student 2
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar_4,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar_3),
           axislabcol = "grey",
           vlcex = 2,
           pcol = c("#04cae0", "#e004d1"), # Customizando o poligono
           pfcol = scales::alpha(c("#04cae0", "#e004d1"), 0.1),
           plwd = 2,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1
           # title = "Patient IV"
)

# Patient 5 -----------------------------------------------------------------------------------

# Sumarizando as variaveis que serãp plotadas
score_5 <- df |>
  filter(ID == 5,
         questionario == "podci") |>
  select(
    -ID,
    -time,
    -questionario,
    -Physical,
    -Emotional,
    -Social,
    -School,
    -Psychosocial,
    -"Global PedsQL") |>
  as.data.frame()

row.names(score_5) <- c("PRE", "POST") # inserindo o nome das linhas

score_5


# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar_5 <- rbind(max_min, score_5)

df_radar_5

# Plot the data for student 2
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar_5,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar_3),
           axislabcol = "grey",
           vlcex = 2,
           pcol = c("#04cae0", "#e004d1"), # Customizando o poligono
           pfcol = scales::alpha(c("#04cae0", "#e004d1"), 0.1),
           plwd = 2,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1
           # title = "Patient V"
)

# Patient 6 -----------------------------------------------------------------------------------

# Sumarizando as variaveis que serãp plotadas
score_6 <- df |>
  filter(ID == 6,
         questionario == "podci") |>
  select(
    -ID,
    -time,
    -questionario,
    -Physical,
    -Emotional,
    -Social,
    -School,
    -Psychosocial,
    -"Global PedsQL") |>
  as.data.frame()

row.names(score_6) <- c("PRE", "POST") # inserindo o nome das linhas

score_6


# Juntando as variaveis sumarizadas com os escores minimo e máximo
df_radar_6 <- rbind(max_min, score_6)

df_radar_6

# Plot the data for student 2
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))

radarchart(df_radar_6,
           axistype = 1, # Cutomizando as labels
           caxislabels = c(0, 25, 50, 75, 100),
           vlabels = colnames(df_radar_3),
           axislabcol = "grey",
           vlcex = 2,
           pcol = c("#04cae0", "#e004d1"), # Customizando o poligono
           pfcol = scales::alpha(c("#04cae0", "#e004d1"), 0.1),
           plwd = 2,
           plty = 2,
           cglcol = "grey", # Customizando a grid
           cglwd = 0.8,
           cglty = 1
           # title = "Patient VI"
)


# Add an horizontal legend
legend(
  x = 1.3,
  y = 1.2,
  legend = rownames(df_radar_1[-c(1,2),]),
  horiz = TRUE,
  bty = "n",
  pch = 20,
  col = c("#04cae0", "#e004d1"),
  text.col = "black",
  cex = 1,
  pt.cex = 2.5
)
par(op)
