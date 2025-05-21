# Import das librarys 

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Import dos dados

dados <- read.csv("C:/Users/jmrc2/Desktop/dadosecg.csv", sep = ";")

# Como tava a ter problemas por causad da coluna das condições e das ordens decidi adiciona-las manualmente

participantes_condicao <- data.frame(
  PARTICIPANTES = c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11", "P12"),
  CONDICAO = c(1,5,4,2,3,6,1,2,3,4,5,6),
  ORDEM = c("pos_neg_neu","neu_pos_neg", "neg_neu_pos", "pos_neu_neg", "neg_pos_neu", "neu_neg_pos", "pos_neg_neu", "pos_neu_neg", "neg_pos_neu", "neg_neu_pos", "neu_pos_neg", "neu_neg_pos")
)

# Juntamos tudo

dados <- dados %>% left_join(participantes_condicao, by = "PARTICIPANTES")

# Organizamos por condição

dados_por_condicao <- dados %>% arrange(CONDICAO)

# E agora metemos para ficar por ordem numérica e por condição

dados_por_condicao <- dados %>% mutate(CONDICAO = as.numeric(CONDICAO)) %>% arrange(CONDICAO) %>% group_by(CONDICAO, ORDEM)

# Calcular as médias por condição para os 2 participantes de cada condição

medias_por_condicao <- dados %>% group_by(CONDICAO) %>% summarise(
  MediaVPOS = mean(VPOS, na.rm = TRUE)
  DPVPOS = mean(STDDEV_VPOS, na.rm = TRUE)
  MediaVNEG = mean(VNEG, na.rm = TRUE)
  DPVNEG = mean(STDDEV_VNEG, na.rm = TRUE)
  MEDIAVNEU = mean(VNEU, na.rm = TRUE)
  DPVNEU = mean(STDDEV_VNEU, na.rm = TRUE)
  
  MediaTRPOS = mean(TRPOS, na.rm = TRUE)
  DPTRPOS = mean(STDDEV_TRPOS, na.rm = TRUE)
  MediaTRNEG = mean(TRNEG, na.rm = TRUE)
  DPTRNEG = mean(STDDEV_TRNEG, na.rm = TRUE)
  MediaTRNEU = mean(TRNEU, na.rm = TRUE)
  DPTRNEU = mean(STDDEV_TRNEU, na.rm = TRUE)
)

print(medias_por_condicao)

# Médias gerais de todos os participantes

medias_gerais <- dados %>% sumarise(
  MediaVPOS = mean(VPOS, na.rm = TRUE)
  DPVPOS = sd(STDDEV_VPOS, na.rm = TRUE) # sd calcula o desvio padrão no R
  MediaVNEG = mean(VNEG, na.rm = TRUE)
  DPVNEG = sd(STDDEV_VNEG, na.rm = TRUE)
  MEDIAVNEU = mean(VNEU, na.rm = TRUE)
  DPVNEU = sd(STDDEV_VNEU, na.rm = TRUE)
  
  MediaTRPOS = mean(TRPOS, na.rm = TRUE)
  DPTRPOS = sd(STDDEV_TRPOS, na.rm = TRUE)
  MediaTRNEG = mean(TRNEG, na.rm = TRUE)
  DPTRNEG = sd(STDDEV_TRNEG, na.rm = TRUE)
  MediaTRNEU = mean(TRNEU, na.rm = TRUE)
  DPTRNEU = sd(STDDEV_TRNEU, na.rm = TRUE)
)

print(medias_gerais)

# Media dos BPMs durante os vídeos por participante

bpms <- dados %>% select(Sujeito = PARTICIPANTES, Condição = CONDICAO, POS = VPOS, NEG = VNEG, NEU = VNEU, SD_POS = STDDEV_VPOS, SD_NEG = STDDEV_VNEG, SD_NEU = STDDEV_VNEU) %>%
                  pivot_longer(cols = c(POS,NEG,NEU), names_to = "Video", values_to = "BPM") %>%
                  mutate(SD = case_when(Video == "POS" ~ SD_POS, Video  == "NEG" ~ SD_NEG, Video == "NEU" ~ SD_NEU))

ggplot(bpms, aes(x = Video, y = BPM, group = Sujeito)) + 
  geom_line(aes(color = Sujeito), linewidth = 1) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = BPM - SD, ymax = BPM + SD), width = 0.2) +
  facet_wrap(~ Sujeito) +
  labs(title = "BPM por tipo de vídeo e por participante", y = "BPM", x = "Vídeo") +
  theme_minimal()

# Media dos BPMs durante os vídeos por participante

bpms_geral <- bpms %>% group_by(Video) %>% summarise(Media = mean(BPM, na.rm = TRUE), DP = sd(BPM, na.rm = TRUE)) %>% rename (emocao = Video)

ggplot(bpms_geral, aes(x = emocao, y = Media, group = 1)) + 
  geom_line(color = "blue") +
  geom_point(color = "blue") + 
  geom_errorbar(aes(ymin = Media - DP, ymax = Media + DP), width = 0.1) +
  scale_y_continuous(limits = c(86, 88)) + 
  labs(title = "Média Geral dos BPMS nos videos", y = "BPM", x = "Tipo de Estímulo") + 
  theme_minimal()

# Media dos BPMs durante os testes do tempo de reação por participante

trs <- dados %>% select(Sujeito = PARTICIPANTES, Condição = CONDICAO, POS = TRPOS, NEG = TRNEG, NEU = TRNEU, SD_POS = STDDEV_TRPOS, SD_NEG = STDDEV_TRNEG, SD_NEU = STDDEV_TRNEU) %>%
  pivot_longer(cols = c(POS,NEG,NEU), names_to = "Video", values_to = "TR") %>%
  mutate(SD = case_when(Video == "POS" ~ SD_POS, Video  == "NEG" ~ SD_NEG, Video == "NEU" ~ SD_NEU))

ggplot(trs, aes(x = Video, y = TR, group = Sujeito)) + 
  geom_line(aes(color = Sujeito), linewidth = 1) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = TR - SD, ymax = TR + SD), width = 0.2) +
  facet_wrap(~ Sujeito) +
  labs(title = "BPM nos Tempos de Reação por participante", y = "BPM", x = "Vídeo") +
  theme_minimal()

# Media Geral dos BPMs durante os testes do tempo de reação 

trs_geral <- trs %>% group_by(Video) %>% summarise(Media = mean(TR, na.rm = TRUE), DP = sd(TR, na.rm = TRUE)) %>% rename (emocao = Video)

ggplot(trs_geral, aes(x = emocao, y = Media, group = 1)) + 
  geom_line(color = "red") +
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = Media - DP, ymax = Media + DP), width = 0.1) +
  scale_y_continuous(limits = c(86, 88)) + 
  labs(title = "Média Geral dos BPMS nos TRs", y = "BPM", x = "Tipo de Estímulo") + 
  theme_minimal()

# Media dos BPMs nos videos com base nas condições

medias_bpm <- medias_por_condicao %>% select(CONDICAO, starts_with("MediaV")) %>% pivot_longer(cols = starts_with("MediaV"), names_to = "emocao", values_to = "Media") %>% mutate(emocao = case_when(emocao == "MediaVPOS" ~ "Positivo", emocao == "MediaVNEG" ~ "Negativo", emocao == "MediaVNEU" ~ "Neutro"))

ggplot(medias_bpm, aes (x = emocao, y = Media, group = factor(CONDICAO), color = factor(CONDICAO))) +
  geom_point(size = 2) +
  geom_smooth(se = False, method = "loess") + 
  labs (title = "Médias de BPMs nos videos por Condicão", x = "Tipo de Estímulo", y = "BPM", color = "Condição") +
  theme_minimal(base_size = 13)

# Medias dos BPMs nos Tempos de Reação com base nas condições

medias_tr <- medias_por_condicao %>% select(CONDICAO, starts_with("MediaTR")) %>% pivot_longer(cols = starts_with("MediaTR"), names_to = "emocao", values_to = "Media") %>% mutate(emocao = case_when(emocao == "MediaTRPOS" ~ "Positivo", emocao == "MediaTRNEG" ~ "Negativo", emocao == "MediaTRNEU" ~ "Neutro"))

ggplot(medias_tr, aes (x = emocao, y = Media, group = factor(CONDICAO), color = factor(CONDICAO))) +
  geom_point(size = 2) +
  geom_smooth(se = False, method = "loess") + 
  labs (title = "Médias de BPMs nos Tempos de Reação por Condicão", x = "Tipo de Estímulo", y = "BPM", color = "Condição") +
  theme_minimal(base_size = 13)

# Testes ANOVA

# ANOVA para os videos 

anova_videos <- aov(BPM ~ Condição * Video + Error(Sujeito/Video), data = bpms)
summary(anova_videos)

# ANOVA para os Tempos de Reação

anova_tr <- aov(TR ~ Condição * Video + Error(Sujeito/Video), data = trs)
summary(anova_tr)

# Import dos dados do questionário

dadosq <- read.csv("C:/Users/Utilizador/Desktop/dadosq.csv", sep = ";")

# Testes de correlação para verificar se os a opinião dos videos e os batimentos cardiacos estão relacionados

cor.test(dados$VPOS, dadosq$VPOS)
cor.test(dados$VNEG, dadosq$VNEG)
cor.test(dados$VNEU, dadosq$VNEU)
