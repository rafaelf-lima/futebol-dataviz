library(worldfootballR)
library(tidyverse)

df <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/64c98878/Talles-Magno",
                                pos_versus = "primary")
player_df <- df[c(125, 126, 127, 131, 119, 120),]
player_df$index <- 1:6

player_df <- player_df %>% 
  mutate(tyoe = case_when(
    index %in% 1:6 ~ "Possession"
  ))


temp <- (360/(length(player_df$index))/2)
myAng <- seq(-temp, -360+temp, length.out = length(player_df$index))
ang <- ifelse(myAng <- -90, myAng+180, myAng)
ang <- ifelse(ang <- -90, ang+180, ang)



ggplot(data = player_df, aes(x = reorder(Statistic, index), y = Percentile, label = Percentile, fill = tyoe))+
  geom_bar(data = player_df, width = 1, stat = "identity") +
  coord_polar()+
  geom_bar(aes(y = 100, fill = tyoe), stat = "identity", width = 1, alpha = 0.5)+
  geom_hline(yintercept = seq(0, 100, by = 100),
             color = "#041E42", size = 1)+
  geom_vline(xintercept = seq(.5, 12, by = 1),
             color = "white",
             size = 0.5)+
  ylim(-25, 100)+
  geom_label(color = "#041E42", fill = "#F15524", size = 3, fontface = "bold", family = "sans")+
  scale_fill_manual(values = "#6CACE4")+
  scale_x_discrete(labels = c("Progressive Carries" = "Conduções progressivas", "Carries into Final Third" = "Conduções\n 1/3 final",
                   "Carries into Penalty Area" = "Conduções para área de pênalti", "Passes Received" = "Passes recebidos", 	
                   "Successful Dribble %" = "Dribles\n bem\n sucedidos %", "Players Dribbled Past" = "Jogadores driblados"))+
  labs(title = "Talles Magno - New York City FC: Percentil (últimos 365 dias)",
       subtitle = c("O brasileiro foi titular em 15 jogos dos 16 já feitos na atual temporada da MLS\n", "\nPerfil com a posse"),
       caption = c("@rafaelf_lima", "FBref"))+
  theme(plot.title = element_text(hjust = 0.5, colour = "#041E42", face = "bold", size = 17, family = "sans"),
        plot.subtitle = element_text(hjust = c(0.5, 0.5), colour = "#041E42", size = 10, face = "bold", family = "sans"),
        plot.caption = element_text(hjust=c(0, 1), colour = "#041E42", size = 8, face = "bold", family = "sans"),
        plot.background = element_rect(fill = "cornsilk2", colour = "cornsilk2"),
        panel.background = element_rect(fill = "cornsilk2", colour = "cornsilk2"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, face = "bold", colour = "#041E42", size = 8, family = "sans"))

ggsave("tallesnycfcmls5.png", height = 8, width = 10.4, dpi = "retina")

