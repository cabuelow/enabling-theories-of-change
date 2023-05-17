# evaluate sensitivity of clustering without conservation spending and ramsar management variables

library(tidyverse)

dat <- read.csv('outputs/final.df.csv')
dat2 <- read.csv('outputs/norams-df.csv') %>%
  rename('Cluster_norams' = 'Enabling.profile')
dat3 <- read.csv('outputs/nospend-df.csv') %>%
  rename('Cluster_nospend' = 'Enabling.profile')

dat4 <- select(dat, Country, Enabling.profile) %>%
  left_join(select(dat2, Country, Cluster_norams)) %>%
  left_join(select(dat3, Country, Cluster_nospend))

# make a dataframe for each country that reports a 1 if in the same cluster or a 0 if not in the same cluster
# statistic tells us the proportion of times a country was not in the same group as a country if it

df <- data.frame(Country = NA, `No Ramsar Management` = NA, `No Conservation spending` = NA)
for(i in 1:nrow(dat)){
  dat5 <- dat4 %>%
    mutate(Enabling.profile = ifelse(Enabling.profile == dat4[i,'Enabling.profile'], 1, 0),
           Cluster_norams = ifelse(Cluster_norams == dat4[i,'Enabling.profile'], 1, 0),
           Cluster_nospend = ifelse(Cluster_nospend == dat4[i,'Enabling.profile'], 1, 0)) %>%
    mutate(mean_norams = (Enabling.profile + Cluster_norams)/2,
           mean_nospend = (Enabling.profile + Cluster_nospend)/2)
  df[i,1] <- dat4[i, 'Country']
  df[i,2] <- nrow(filter(dat5, mean_norams == 0.5))/nrow(dat5)
  df[i,3] <- nrow(filter(dat5, mean_nospend == 0.5))/nrow(dat5)
}

# histogram

df %>%
  pivot_longer(cols = No.Ramsar.Management:No.Conservation.spending,
               names_to = 'indicator', values_to = 'stat') %>%
  ggplot(aes(x = stat)) +
  geom_histogram(binwidth = 0.01) +
  xlab('Proportion of country pairwise clustering differences relative to full model')+
  ylab('Number of countries') +
  facet_wrap(~indicator) +
  theme_classic()

ggsave('outputs/pairwise-clustering-differences.png', width = 6, height = 2)

# boxplot

df %>%
  pivot_longer(cols = No.Ramsar.Management:No.Conservation.spending,
               names_to = 'indicator', values_to = 'stat') %>%
  ggplot(aes(x = indicator, y = stat)) +
  geom_violin() +
  xlab('')+
  ylab(paste0("Proportion of country", "\n", "pairwise clustering differences")) +
  theme_classic()

ggsave('outputs/pairwise-clustering-differences_violin.png', width = 4, height = 3)
