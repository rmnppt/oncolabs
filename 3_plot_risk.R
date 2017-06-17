#Plot risks
library(ggplot2)

#create bar plot of sarah's risk vs perfectly healthy individual in sarahs age

sarah.plot.df <- data.frame(Individual = c("Age Healthy Risk", "Sarah's Risk"), Risk = c(sarah.n.risk, sarah.risk))

sarah = ggplot(sarah.plot.df, aes(Individual, Risk)) +
  geom_col(aes(fill = Individual),width = 0.6) + scale_fill_manual(values=c("#8393ca", "#ed1456"), guide=FALSE) + 
  ylim(0,50) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=16),
                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))


alan.plot.df <- data.frame(Individual = c("Age Healthy Risk", "Tim's Risk"), Risk = c(alan.n.risk, alan.risk))

tim = ggplot(alan.plot.df, aes(Individual, Risk)) +
  geom_col(aes(fill = Individual),width = 0.6) + scale_fill_manual(values=c("#8393ca", "#ed1456"), guide=FALSE) + 
  ylim(0,50) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=16),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))

sarah

tim
