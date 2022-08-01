library(tidyverse)
library(worldfootballR)

df <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/b40424f1/Junior-Urso",
                                pos_versus = "primary")

df_player <- df[c(125, 126, 131, 43, 44, 47, 117),]
df_player$index <- 1:7

df_player <- df_player %>% 
  mutate(p = case_when(
    index %in% 1:7 ~ "Possession"
  ))


ggplot(df_player, aes(x = reorder(Statistic, index), y = Percentile, label = Percentile, fill = p))+
  geom_bar(data = df_player, width = 1, stat = "identity")+
  coord_polar()+
  geom_bar(aes(y = 100, fill = p), stat = "identity", width = 1, alpha = .5, fill = "#633492")+
  geom_hline(yintercept = seq(0,100, by = 100),
             color = "#FDE192", size = 1)+
  geom_vline(xintercept = seq(.5, 12, by = 1),
            color = "#FDE192",
            size = 1)+
  ylim(-20, 100)+
  geom_label(colour = "#633492", fill = "#FDE192", size = 3, family = "sans", fontface = "bold")+
  scale_fill_manual(values = "#633492")+
  scale_x_discrete(labels = c("Progressive Carries" = "Conduções em progressão", "Key Passes" = "Passes-chave", "Dribbles Completed" = "Dribles completos",
                              "Progressive Passes" = "Passes\nem\nprogressão", "Passes into Final Third" = "Passes ao\n terço final",
                              "Carries into Final Third" = "Conduções\n ao\nterço\nfinal", "Passes Received" = "Passes\n recebidos"))+
  labs(title = "Júnior Urso - Orlando City: Percentil (últimos 365 dias)",
       subtitle = c("Idade: 33 | Posição: Volante | País: Brasil | Pé: Direito", "\nÚltimos clubes: Shandong Luneng, Atlético-MG, Guangzhou R&F, Corinthians e Orlando City", "\nPerfil com a posse"),
       caption = c("@rafaelf_lima", "Fonte: StatsBomb, FBref"))+
  theme(plot.title = element_text(hjust = 0.5, colour = "black", face = "bold", size = 17, family = "sans"),
        plot.subtitle = element_text(hjust = c(0.5, 0.5, 0.5), vjust = c(1.2, 1.1, 0.8), colour = "black", face= "bold", size = c(12, 12, 15), family = "sans"),
        plot.caption = element_text(hjust = c(0.2, 0.8), colour = "black", face = "bold", size = 8, family = "sans"),
        plot.background = element_rect(colour = "#cccccc", fill = "#cccccc"),
        legend.position = "none",
        panel.background = element_rect(colour = "#cccccc", fill = "#cccccc"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, face = "bold", colour = "black", size = 8, family = "sans"))


ggsave("juniorursomlsposse10.png", height = 8, width = 9.9, dpi = "retina")
                   