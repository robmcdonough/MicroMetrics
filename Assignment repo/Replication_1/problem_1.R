library(pacman)
p_load(data.table, estimatr, zeallot, lfe, tidyverse, parallel,
       tidyr, reshape2, viridis, RColorBrewer, plyr, dplyr, purrr, furrr, here, ggplot2, ggthemes)

better_late=fread("Replication_1/Better_Late_Than_Never.csv")



##part (a)
better_late_treat=better_late[D==1, c(1,2,3,4,5)]
better_late_control=better_late[D==0,c(1,2,3,4,5)]


better_late_treat=melt(better_late_treat, id.vars = c("V1","y","T") ,measure.vars = c("Y1", "Y0"))
better_late_control=melt(better_late_control, id.vars = c("V1","y","T") ,measure.vars = c("Y1", "Y0"))

# Interleaved histograms
treated_hist=ggplot(better_late_treat, aes(x=value, color=variable)) +
  geom_histogram(fill="white", position= position_dodge2(width=.5), binwidth = .1,)+
  theme(legend.position="top",plot.title = element_text(hjust = 0.5))+
  labs(title = "Treated and Control Outcomes for Treated Observations",colour="Outcome")+
  theme_clean()
treated_hist


control_hist=ggplot(better_late_control, aes(x=value, color=variable)) +
  geom_histogram(fill="white", position= position_dodge2(width=.5), binwidth = .1,)+
  theme(legend.position="top",plot.title = element_text(hjust = 0.5))+
  labs(title = "Treated and Control Outcomes for Control Observations",colour="Outcome")+
  theme_clean()
control_hist

##part (b)

better_late_reality=better_late[,c("V1","y","D")][,D:=as.character(D)]
names(better_late_reality)[2]="outcome"

reality_hist=ggplot(better_late_reality, aes(x=outcome, color=D)) +
  geom_histogram(fill="white",position= position_dodge2(width=.5), binwidth = .1,)+
  theme(legend.position="top",plot.title = element_text(hjust = 0.5))+
  labs(title = "Observed Outcomes Divided by Treatment Status",colour="Outcome")+
  theme_clean()
reality_hist

## part (c)
names(better_late)[5]="effect"

ATE=better_late[,.(ATE=mean(effect))]




##part (d)


reggy=lm(outcome ~ D, data = better_late_reality)

summary(reggy)
