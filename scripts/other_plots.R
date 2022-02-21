EDA = list()
EDA$head = head(data)
EDA$skim = skim(data)
save(EDA,file = "models/EDA.rda")


# Poisson Deviance Loss
PDL = data.frame(predicted = 0:50,
           actual = 20) %>% 
  mutate(PDL = actual*log(actual / predicted) - (actual - predicted)) %>% 
  ggplot(aes(x = predicted, y = PDL))+
  geom_line(size = 1)+
  ggtitle("Poisson Deviance Loss function",
          subtitle = "20 - actual observation")+
  xlab("Model Prediction")+
  ylab("Poisson Deviance Loss")+
  # theme_minimal()+
  theme_light()+
  theme(text=element_text(family="serif"),
        legend.justification = c("right", "top"))

ggsave(plot = PDL,
       path = "graphs", 
       filename = "Poisson_Deviance_Loss.png",
       device = "png")

# EDA
EDA = data %>% 
  group_by(DrivAge,Area) %>% 
  summarise(Freq = mean(ClaimNb),
            BonusMalus = mean(BonusMalus)) %>% 
  ggplot(aes(x = DrivAge, y=Freq, color = BonusMalus))+
  facet_wrap(~Area)+
  geom_point()+
  ggtitle("Claim frequency by driver age and region")+
  xlab("Driver Age")+
  ylab("")+
  theme_light()+
  theme(text=element_text(family="serif"),
        legend.justification = c("right", "top"))


ggsave(plot = EDA,
       path = "graphs", 
       filename = "EDA.png",
       device = "png")


