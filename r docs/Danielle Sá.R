###################Projeto Fantasma##############

install.packages("tidyverse")
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

a2 <- a2 %>%
  mutate(season = recode(season, `1` = "1ª"))
a2 <- a2 %>%
  mutate(season = recode(season, `2` = "2ª"))
a2 <- a2 %>%
  mutate(season = recode(season, `3` = "3ª"))
a2 <- a2 %>%
  mutate(season = recode(season, `4` = "4ª"))


ggplot(a2) +
  aes(x = reorder(season, imdb), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "imdb") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")


                               ###Análise 3###

# Análise 3: Top 3 terrenos mais frequentes pela ativação da armadilha

banco <- banco %>%
  mutate(trap_work_first = recode(trap_work_first, "False" = "Falso"))

banco <- banco %>%
  mutate(trap_work_first = recode(trap_work_first, "True" = "Verdadeiro"))

dados_limpos <- banco[banco$trap_work_first != "", ]

frequencia <- table(dados_limpos$setting_terrain, dados_limpos$trap_work_first)
frequencia

frequencia_df <- as.data.frame(frequencia)
colnames(frequencia_df) <- c("Terreno", "Armadilha_Funcionou", "Frequencia")

frequencia_df <- frequencia_df[order(-frequencia_df$Frequencia), ]

top_terrenos <- by(frequencia_df, frequencia_df$Armadilha_Funcionou, head, n = 3)
print(top_terrenos)

topterrenos <- do.call(rbind, top_terrenos)


top_terrenos_df <- topterrenos %>%
  mutate(
    freq_relativa = round(Frequencia / sum(Frequencia) * 100, 1)
  )

porcentagens <- str_c(top_terrenos_df$freq_relativa, "%") %>%
  str_replace("\\.", ",")

legendas <- str_squish(str_c(top_terrenos_df$Frequencia, " (", porcentagens, ")"))

top_terrenos_df <- top_terrenos_df %>%
  mutate(Armadilhas = trimws(tolower(Armadilhas)))

top_terrenos_df <- top_terrenos_df %>%
  mutate(Armadilhas = case_when(
    Armadilhas == "true" ~ "Verdadeiro",
    Armadilhas == "false" ~ "Falso",
    TRUE ~ Armadilhas  
  ))

top_terrenos_df <- top_terrenos_df %>%
  rename(Armadilhas = Armadilha_Funcionou)

ggplot(top_terrenos_df) +
  aes(
    x = fct_reorder(Terreno, Frequencia, .desc = TRUE), y = Frequencia,
    fill = Armadilhas, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terrenos", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 115, units = "mm")


tabela_contingencia <- table(top_terrenos_df$Armadilhas, top_terrenos_df$Terreno)
teste_qui_quadrado <- chisq.test(tabela_contingencia)

                              ### Análise 4 ###

# Análise 4: Relação entre as notas IMDB e engajamento


ggplot(banco) +
  aes(x = engagement, y = imdb) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.4) +  # Adicione o parâmetro alpha aqui
  labs(
    x = "Engajamento",
    y = "IMDB"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")


cor.test(banco$imdb, banco$engagement, method = "pearson")


medidas_resumo <- banco %>%
  summarise(
    Mediana = median(engagement),
    Q1 = quantile(engagement, probs = 0.25),
    Q3 = quantile(engagement, probs = 0.75),
    Minimo = min(engagement),
    Maximo = max(engagement),
    Media = mean(engagement),
    Desvio_Padrao = sd(engagement),
    Variancia = var(engagement)
  ) %>%
  ungroup()

medidas_resumo <- banco %>%
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

                                ###Análise 5###

#Análise 5:Variação da nota de engajamento pelo personagem que conseguiu capturar o
#monstro


scooby <- banco %>%
  select(caught_fred, caught_daphnie, caught_velma, caught_shaggy, caught_scooby, caught_other, caught_not, engagement)

scooby <- scooby %>%
  rename(Ninguém = caught_not)

dados_longos <- melt(scooby, id.vars = "engagement")


scooby1 <- subset(dados_longos, value == "True")


ggplot(scooby1) +
  aes(x = reorder(variable, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagens", y = "Engajamento") +
  theme_estat()
ggsave("box_bii.pdf", width = 158, height = 93, units = "mm")


medidas_resumo5 <- scooby1 %>%
  group_by(variable) %>%
  summarise(
    Mediana = median(engagement),
    Q1 = quantile(engagement, probs = 0.25),
    Q3 = quantile(engagement, probs = 0.75),
    Minimo = min(engagement),
    Maximo = max(engagement),
    Media = mean(engagement),
    Desvio_Padrao = sd(engagement),
    Variancia = var(engagement)
  ) %>%
  ungroup()




