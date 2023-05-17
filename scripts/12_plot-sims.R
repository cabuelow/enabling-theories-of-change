library(tidyverse)

fils <- list.files('outputs/', pattern = 'sim_', full.names = T)
dat <- do.call(rbind, lapply(fils, read.csv))

dat %>%
  filter(r2>0.97) %>% # filter only for models that were a good fit when data weren't missing
  group_by(corr, prop.na) %>%
  summarise(n = n(),
            r2_median = median(r2),
            RMSE_median = median(RMSE),
            r2_mnar_median = median(r2_mnar),
            r2_sd = sd(r2, na.rm = T),
            RMSE_sd = sd(RMSE, na.rm = T),
            RMSE_mnar_median = median(RMSE_mnar),
            r2_mnar_sd = sd(r2_mnar, na.rm = T),
            RMSE_mnar_sd = sd(RMSE_mnar, na.rm = T)) %>%
  ggplot() +
  geom_errorbar(aes(x = factor(prop.na),
                    ymin = r2_mnar_median - r2_mnar_sd, ymax = r2_mnar_median + r2_mnar_sd),
                width = 0.001, col = 'grey') +
  geom_point(aes(x = factor(prop.na), y = r2_mnar_median)) +
  ylab(bquote('R'^2)) +
  xlab('Proportion of missing values') +
  ylim(c(0,1.01)) +
  theme_classic()

ggsave('outputs/sim-study-mnar.png', width = 5, height = 3)

