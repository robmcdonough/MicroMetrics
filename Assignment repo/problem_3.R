library(pacman)
p_load(data.table, estimatr, zeallot, lfe, tidyverse, parallel,
       tidyr, reshape2, viridis, RColorBrewer, plyr, dplyr, purrr, furrr, here, ggplot2, ggthemes)


bac_data=fread("Replication_1/BAC_deiden.csv")
head(bac_data)
bac_data[,bac:=run/1000]
## (a)
hist_bac=ggplot(bac_data, aes(x=bac)) +
  geom_histogram(binwidth = .002, fill="red", color="black", size=.05)+
  theme(legend.position="top",plot.title = element_text(hjust = 0.2))+
  geom_vline(xintercept = 0.08, size=0.001, color="springgreen")+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "BAC Distribution of those Pulled Over")+
  theme_clean()
hist_bac

bac_data[,bac_rescaled:=bac-0.08][,drunk:=ifelse(bac>=.08,1,0)]

bac_data_bandwidth=bac_data[bac_rescaled>-0.05 & bac_rescaled< 0.05,]

control_reg1= lm(aged ~ drunk + bac_rescaled + I(drunk*bac_rescaled), data=bac_data_bandwidth)

control_reg2= lm(male ~ drunk + bac_rescaled + I(drunk*bac_rescaled), data=bac_data_bandwidth)

control_reg3= lm(acc ~ drunk + bac_rescaled + I(drunk*bac_rescaled), data=bac_data_bandwidth)

control_reg4= lm(white ~ drunk + bac_rescaled + I(drunk*bac_rescaled), data=bac_data_bandwidth)


summary(control_reg1)

summary(control_reg2)


summary(control_reg3)


summary(control_reg4)



rec_reg4= lm(recidivism ~ drunk + bac_rescaled + I(drunk*bac_rescaled), data=bac_data_bandwidth)

summary(rec_reg4)



bac_data_binned=bac_data[,mean(recidivism),by=]

movies


binned=bac_data[,bac_bin:=cut_interval(bac,length = 0.002)]

binned_means=binned[,.(rec_mean=mean(recidivism), bin_val=min(bac)), by=bac_bin]

rec_graph=ggplot(data=binned_means, aes(bin_val, rec_mean))



rec_graph+
  geom_point()+
  geom_segment()
#break data into two segments based on threshold to plot lines of best fit for each part
