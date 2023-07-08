# Run a cluster analysis on the predicted standard normal indicator values to classify countries in to Enabling Profiles and produce a map

library(tidyverse)
library(factoextra)
library(tmap)
library(sf)
library(clValid)
library(dendextend)
library(RColorBrewer)
library(scales)
sf_use_s2(FALSE)

# dat

resid.df <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final.csv')
dat <- read.csv('data/master-df_final.csv')
World <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries_simp.gpkg')
World$Country <- recode(World$Country, `Russian Federation` = 'Russia',
                        `Brunei Darussalam` = 'Brunei',
                        Comoros = 'Comores',
                        `Timor-Leste` = 'East Timor',
                        Somalia = 'Federal Republic of Somalia',
                        `Côte d'Ivoire` = 'Ivory Coast',
                        Mauritius = 'Republic of Mauritius')

# rescale predicted indicator values to smallest range

max <- min(apply(resid.df, 2, max))
min <- max(apply(resid.df, 2, min))

resid.df <- resid.df %>%
  mutate_all(funs(rescale(., to = c(min, max))))

set.seed(444)

# silhouette width to assess quality of clustering configurations
# assess between five and ten clusters

fviz_nbclust(resid.df, pam, method = "silhouette", k.max = 10) +
  theme_classic()
#fviz_nbclust(resid.df, pam, method = "wss", k.max = 10) +
 # theme_classic()
#fviz_nbclust(resid.df, pam, method = "gap_stat", k.max = 10) +
 #theme_classic()

# run cluster analysis
# Note that six clusters was chosen after visually inspecting all clustering configurations between five and ten

df.clust <- cluster::pam(resid.df, 6, metric = 'euclidean')

# get cluster medoids

clust.med <- df.clust$medoids

# hierarchical cluster analysis on cluster medoids

h.clust <- hclust(dist(clust.med, method = 'euclidean'), method = "ward.D")

# dendrogram

fviz_dend(h.clust)
dend <- as.dendrogram(h.clust)

# re-label leaves according to hierarchy

clust.name <- data.frame(cluster = c(1:nrow(clust.med)))
clust.name$fine.clust <- as.factor(c(3,6,1,5,4,2))
clust.name <- clust.name %>% arrange(fine.clust)

# get color palette and save new cluster order

pal <- c("#88CCEE","#6699CC", "#44AA99", "#117733", "#CC6677","#882255")
clust.name$pal.fine <- pal
write.csv(clust.name, 'outputs/clus-new-order.csv', row.names = F)

# plot and save dendrogram

labels(dend) <- clust.name$fine.clust
labels <- labels(dend)
dend %>% set("leaves_pch", 19) %>% set("leaves_cex", 1.5) %>% set("leaves_col", as.character(clust.name$pal.fine)) %>% rotate_DendSer %>% plot()

png('outputs/dendrogram.png', width = 500, height = 200)
dend %>% set("leaves_pch", 19) %>% set("leaves_cex", 1.5) %>% set("leaves_col", as.character(clust.name$pal.fine)) %>% plot()
dev.off()

# get sites and clusters and save

clusters <- data.frame(ISO_SOV1 = dat$ISO_SOV1, Cluster = df.clust$cluster)
write.csv(clusters, 'outputs/cluster.csv', row.names = F)

# get full spatial df with model results and merge by country

dat2 <- dat %>%
  left_join(clusters, by = 'ISO_SOV1') %>%
  distinct() %>%
  dplyr::rename(cluster = Cluster) %>%
  left_join(clust.name, by = 'cluster') %>%
  dplyr::rename('Enabling profile' = fine.clust)

world.clust <- world.clust <- World %>%
  left_join(dat2, by = 'Country') %>%
  filter(!is.na(pal.fine)) %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>%
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

World <- World %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>%
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

# plot

tmap_mode('plot')

m <- tm_shape(World) +
  tm_fill(col = 'lightgrey') +
  tm_shape(world.clust) +
  tm_fill(col = 'Enabling profile', style = 'cat',
          palette = pal,
          legend.is.portrait=FALSE) +
  tm_borders(col = 'white', lwd = 0.05) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.position = c(0.5, 0.8),
            legend.title.size = 0.9,
            legend.text.size = 0.8,
            frame = FALSE)
m

tmap_save(m, 'outputs/Fig1A.png', width = 8, height = 6)

