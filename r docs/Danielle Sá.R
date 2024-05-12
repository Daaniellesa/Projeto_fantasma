###################Projeto Fantasma##############

library(tidyverse)
banco <- read.csv("banco_final.csv")


estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

                           ###Análise 1###

# Análise 1:Número de lançamentos a cada década por formato de lançamento

banco <- banco %>%
  mutate(date_aired = as.Date(date_aired, format = "%Y-%m-%d"))

banco <- banco %>%
  mutate(AnoLancamento = year(date_aired),  
         Década = floor(AnoLancamento / 10) * 10)

lançamentos <- banco %>%
  group_by(Década, format) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round(freq / sum(freq) * 100, 1))

lançamentos <- lançamentos %>%
  rename(Formato = format)

lançamentos <- lançamentos %>%
  mutate(Formato = recode(Formato,
                          "Movie" = "Filme"))

ggplot(lançamentos) +
  aes(x = Década, y = freq, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Frequência") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")


                 ###Análise 2###


# Análise 2:Variação da nota IMDB por temporada dos episódios

a2 <- subset(banco, season != 'Movie')
a2 <- subset(a2, season != 'Special')
a2 <- subset(a2, season != 'Crossover')

medidas_resumo <- a2 %>%
  group_by(season) %>%
  summarise(
    Mediana = median(imdb),
    Q1 = quantile(imdb, probs = 0.25),
    Q3 = quantile(imdb, probs = 0.75),
    Minimo = min(imdb),
    Maximo = max(imdb),
    Media = mean(imdb),
    Desvio_Padrao = sd(imdb),
    Variancia = var(imdb)
  ) %>%
  ungroup()

print(levels(a2$season))

a2 <- a2[order(a2$season), ]

ggplot(a2) +
  aes(x = reorder(season, imdb, FUN = median), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "imdb") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")


                    ###Análise 3###

# Análise 3: Top 3 terrenos mais frequentes pela ativação da armadilha

dados_limpos <- banco[banco$trap_work_first != "", ]

frequencia <- table(dados_limpos$setting_terrain, dados_limpos$trap_work_first)
frequencia
frequencia_df <- as.data.frame(frequencia)
colnames(frequencia_df) <- c("Terreno", "Armadilha_Funcionou", "Frequencia")
frequencia_df <- frequencia_df[order(-frequencia_df$Frequencia), ]
top_terrenos <- by(frequencia_df, frequencia_df$Armadilha_Funcionou, head, n = 3)
print(top_terrenos)
.
