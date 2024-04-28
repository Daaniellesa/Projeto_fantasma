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

porcentagens <- paste(lançamentos$freq_relativa, "%")
legendas <- paste(lançamentos$freq, " (", porcentagens, ")")

lançamentos$Década <- as.character(lançamentos$Década)


ggplot(lançamentos) +
  aes(
    x = fct_reorder(Década, freq, .desc = TRUE), 
    y = freq,
    fill = format, 
    label = porcentagens
  ) +
  geom_col(position = "dodge") +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.3, hjust = 0.3,
    size = 3
  ) +
  labs(
    x = "Décadas",
    y = "Frequência de Lançamentos",
    fill = "Formato de Lançamento"
  ) +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")
