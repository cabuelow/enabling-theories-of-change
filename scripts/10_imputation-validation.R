# validate interpolated predictions with independent variables

library(tmap)
library(tidyverse)
library(patchwork)
data('World') # happy planet index from the year 2012

dat <- read.csv('data/master-df_final.csv')
resid.df <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final_2021.csv')
resid.df$ISO_SOV1 <- dat$ISO_SOV1
resid.df$mean_annual_funds_raw <- dat$mean_annual_funds
resid.df$ramsar.manage_raw <- dat$ramsar.manage

dat3 <- resid.df %>%
  left_join(st_drop_geometry(World), by = c('ISO_SOV1' = 'iso_a3'))

# plot well being index against conservation spending

a <- ggplot() +
  geom_point(data = filter(dat3, !is.na(mean_annual_funds_raw)),
                           aes(x = well_being, y = mean_annual_funds), alpha = 0.75) +
  geom_point(data = filter(dat3, is.na(mean_annual_funds_raw)),
             aes(x = well_being, y = mean_annual_funds), col = 'red', alpha = 0.75) +
  ylab('Conservation spending') +
  xlab('Well-being index') +
  theme_classic()
cor(dat3$well_being, dat3$mean_annual_funds, "pairwise.complete.obs", method = 'spearman')

# plot ecological footprint (i.e., human demand on natural capital) against ramsar management

b <- ggplot() +
  geom_point(data = filter(dat3, !is.na(ramsar.manage_raw)),
             aes(x = footprint, y = ramsar.manage), alpha = 0.75) +
  geom_point(data = filter(dat3, is.na(ramsar.manage_raw)),
             aes(x = footprint, y = ramsar.manage), col = 'red', alpha = 0.75) +
  ylab('Ramsar management') +
  xlab('Ecological footprint') +
  theme_classic()
cor(dat3$footprint, dat3$ramsar.manage, "pairwise.complete.obs", method = 'spearman')

a/b

ggsave('outputs/imputation-validation.png', width = 5, height = 6)
