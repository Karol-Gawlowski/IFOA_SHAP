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

save(eval_list,file = "models/eval_list.rda")