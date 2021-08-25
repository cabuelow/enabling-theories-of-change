# Evaluate robustness of model parameter estimation to violation of ‘missing at random’ assumption for Ramsar Management indicator

library(tidyverse)

resid.df <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final.csv')
resid.df2 <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final-norams.csv')
dat <- read.csv('data/master-df_final.csv')
labels <- read.csv('data/indicator-labels.csv',  fileEncoding = "UTF-8-BOM") %>% 
  dplyr::rename(Indicator = indicator)

# plot predicted indicator values against actual values

resid.df$ISO_SOV1 <- dat$ISO_SOV1
resid.df2$ISO_SOV1 <- dat$ISO_SOV1

# wide to long

resid.df.long <- pivot_longer(select(resid.df, -ramsar.manage), OECD_sc:WGI_RQ_est, 
                              names_to = 'Indicator', values_to = 'Val_resid')
resid.df.long$Indicator <- as.factor(resid.df.long$Indicator)
resid.df.long <- resid.df.long 

resid.df.long2 <- pivot_longer(resid.df2, OECD_sc:WGI_RQ_est, 
                               names_to = 'Indicator', values_to = 'Val_resid2')
resid.df.long2$Indicator <- as.factor(resid.df.long2$Indicator)

dat.plot <- data.frame(resid.df.long, resid.df.long2[,'Val_resid2']) %>% 
  left_join(labels, by = 'Indicator')

# plot

a <- ggplot(dat.plot)+
  geom_point(aes(x = Val_resid, y = Val_resid2)) +
  facet_wrap(.~label, ncol = 4, scales = 'free') +
  ylab('Predictions without ramsar management') +
  xlab('Predictions with ramsar management') +
  theme_classic()
a

ggsave('outputs/compare-predictions-ramsar-manage.png', width = 8, height = 8)
