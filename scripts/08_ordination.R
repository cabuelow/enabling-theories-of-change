# produce an ordination of standard normal indicator values for each country,
# and group by Enabling Profile

# libraries

library(tidyverse)
library(ggrepel)
library(factoextra)
library(scales)

# data

clustdat <- read.csv('outputs/cluster.csv')
inddat <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final_2021.csv')
dat <- read.csv('data/master-df_final.csv')
clust.new <- read.csv('outputs/clus-new-order.csv') %>%
  dplyr::rename(Cluster = cluster)
labels <- read.csv('data/indicator-labels.csv',  fileEncoding = "UTF-8-BOM") %>%
  dplyr::rename(variable = indicator)
clust.final <- clustdat %>%
  left_join(clust.new, by = 'Cluster')

# rescale

max <- min(apply(inddat, 2, max))
min <- max(apply(inddat, 2, min))

inddat <- inddat %>%
  mutate_all(funs(rescale(., to = c(min, max))))

# join country names to indicator data

rownames(inddat) <- dat$Country

# pca

pca <- prcomp(inddat, scale = FALSE)

# function to estimate variance on each principal component

var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}

# compute indicator loadings on each principal component

loadings <- pca$rotation
sdev <- pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev))

# make dataframe for plotting

df <- data.frame(ISO_SOV1 = dat$ISO_SOV1, SOVEREIGN1 = dat$Country,
                 Dev.status = dat$Development_status, PC1 = pca$x[,1],
                PC2 = pca$x[,2])

df2 <- data.frame(variable = rownames(var.coord), PC1 = var.coord[,1], PC2 = var.coord[,2]) %>%
  left_join(labels, by = 'variable')

df.clust <- df %>%
  left_join(clust.final, by = 'ISO_SOV1')

# labels for plotting

df.clust$Dev.status <- as.character(df.clust$Dev.status)
df.clust <- mutate(df.clust, Dev.status = ifelse(Dev.status == 'Least developed', 'Low income', Dev.status))
df.clust$Dev.status <- factor(df.clust$Dev.status, levels = c('High income', 'Upper middle income', 'Lower middle income', 'Low income'))

# calculate stats for reporting

nrow(df.clust %>% filter(fine.clust %in% c(1,2)) %>% filter(Dev.status == 'High income'))/nrow(df.clust %>% filter(fine.clust %in% c(1,2)))
nrow(df.clust %>% filter(fine.clust %in% c(3,4)) %>% filter(Dev.status %in% c('Upper middle income', 'Lower middle income')))/nrow(df.clust %>% filter(fine.clust %in% c(3,4)))
nrow(df.clust %>% filter(fine.clust %in% c(5,6)) %>% filter(Dev.status %in% c('Low income', 'Lower middle income')))/nrow(df.clust %>% filter(fine.clust %in% c(5,6)))

# plot

p <- ggplot(df.clust) +
  geom_point(aes(x = PC1, y = PC2, col = as.factor(fine.clust), shape = as.factor(Dev.status)), size = 3, alpha = 0.8) +
  geom_text_repel(aes(x = PC1, y = PC2, label = SOVEREIGN1), size = 2) +
  scale_color_manual(values = as.character(clust.new$pal.fine)) +
  scale_shape_manual(values = c(15:18)) +
  geom_vline(xintercept = 0, lty = 'dashed') +
  geom_hline(yintercept = 0, lty = 'dashed') +
  xlab('') +
  ylab('') +
  theme_classic() +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = 'lightgrey'),
        panel.border = element_rect(fill = 'transparent'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size= 6),
        legend.box = 'vertical',
        legend.position = 'bottom',
        legend.margin=margin(-20, 0, 0, 0))+
        guides(colour = guide_legend(nrow = 1))
p

ggsave('outputs/FigS6.png', width = 7, height = 7)

# plot with indicators (but will do manually later)

ggplot() + geom_point(data=df2, aes(x = PC1, y = PC2)) +
  geom_text_repel(data =df2, aes(x = PC1, y = PC2, label = label), size = 4) +
  geom_vline(xintercept = 0, lty = 'dashed') +
  geom_hline(yintercept = 0, lty = 'dashed') +
  xlab('') +
  ylab('') +
  theme_classic() +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = 'lightgrey'),
        panel.border = element_rect(fill = 'transparent'),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave('outputs/FigS6-with-indicators.png', width = 8, height = 6)

