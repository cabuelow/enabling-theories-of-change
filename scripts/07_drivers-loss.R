# Identify and map the main drivers of mangrove and seagrass loss in each Enabling Profile

library(tidyverse)
library(sf)
library(RColorBrewer)
library(tmap)
library(EnvStats)

# data

dat <- read.csv('data/goldberg-2020-mangrove-loss.csv', fileEncoding = "UTF-8-BOM") %>% 
  dplyr::rename(iso_a3 = ISO3)
seag.dat <- read.csv('data/seag-country-drivers.csv') %>% 
  dplyr::rename(iso_a3 = ISO_SOV1)
dat3 <- read.csv('data/master-df_final.csv')
clusters <- read.csv( 'outputs/cluster.csv')
clust.new <- read.csv('outputs/clus-new-order.csv') %>% 
  dplyr::rename(Cluster = cluster)
clust.final <- clusters %>% 
  left_join(clust.new, by = 'Cluster')
data('World')

# make a color palette

palette <- c('palevioletred', 'slategray1', 'plum4', 'peachpuff3', 'darkseagreen3')

###### mangroves #######

dat4 <- dat3 %>% 
  left_join(clusters, by = 'ISO_SOV1') %>% 
  distinct() %>% 
  dplyr::rename(iso_a3 = ISO_SOV1) %>% 
  left_join(clust.new, by = 'Cluster') %>% 
  left_join(dat, by = 'iso_a3') %>% 
  filter(!is.na(Country.y))

# calculate proportion of loss to each driver in each cluster/enabling profile

dat5 <- dat4 %>% 
  mutate(total = erosion_loss_km2_10_16 + ewe_loss_km2_10_16 + commodities_loss_10_16 + npc_loss_10_16 + settlement_loss_10_16) %>% 
  mutate(prop.e = erosion_loss_km2_10_16/total,
         prop.ewe = ewe_loss_km2_10_16/total,
         prop.c = commodities_loss_10_16/total,
         prop.npc = npc_loss_10_16/total,
         prop.s = settlement_loss_10_16/total) %>% 
  group_by(fine.clust) %>% 
  summarise_at(vars(prop.e:prop.s), mean, na.rm = T) %>% 
  pivot_longer(prop.e:prop.s, names_to = 'driver', values_to = 'prop')

# calculate number of countries in each profile for plotting

samps <- dat4 %>% 
  select(fine.clust, Country.x) %>% 
  mutate(n = 1) %>% 
  group_by(fine.clust) %>% 
  summarise(n = sum(n)) %>% 
  mutate(label = paste0(fine.clust, '\nn = ',n)) %>% 
  mutate(label2 = paste0('\nn = ',n))

# join the dataframes for plotting

dat5 <- dat5 %>% 
  left_join(samps, by = 'fine.clust')
  
# plot

a <- ggplot(dat5) +
  geom_bar(aes(fill = driver, y = prop, x = as.factor(fine.clust)), stat="identity") +
  scale_fill_manual(values = palette, breaks = c('prop.c', 'prop.e', 'prop.ewe', 'prop.npc', 'prop.s'),
                                    labels = c('Agri/Aquaculture', 'Erosion', 'Extreme weather', 
                                              'Clearing', 'Settlement')) +
  ylab('Proportion of loss') +
  xlab('Enabling profile') +
  stat_summary(aes(x = factor(fine.clust), y = prop, label = label2), fun = mean, geom = "text", size = 2,
               vjust = -5.67) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.position = 'bottom', legend.direction = 'vertical')

a

ggsave('outputs/Fig2B.png', width = 2, height = 3.4)

# join mangrove data with country polygons and plot

world.clust <- World %>% 
  left_join(dat4, by = 'iso_a3') %>% 
  filter(!is.na(Country.y)) %>% 
  st_transform(crs = 4326) %>% 
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>% 
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

World <- World %>% 
  st_transform(crs = 4326) %>% 
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>% 
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

# plot

tmap_mode('plot')

pal <- brewer.pal(9, 'YlGnBu')
pal <- pal[c(2,5,6,8,9)]

m <- tm_shape(World) +
  tm_fill(col = 'lightgrey') +
  tm_shape(world.clust) +
  tm_fill(col = 'fine.clust', style = 'cat', 
          palette = pal,
          legend.is.portrait=FALSE,
          title = 'Enabling Profile') +
  tm_borders(col = 'white', lwd = 0.05) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.position = c(0.5, 0.8),
            legend.title.size = 0.9,
            legend.text.size = 0.8,
            frame = FALSE)
m

tmap_save(m, 'outputs/Fig2A.png', width = 8, height = 6)

###### seagrass #######

seag.dat2 <- data.frame(dat3 %>% 
  left_join(clusters, by = 'ISO_SOV1') %>% 
  dplyr::rename(iso_a3 = ISO_SOV1) %>% 
  left_join(clust.new, by = 'Cluster') %>% 
  inner_join(seag.dat, by = 'iso_a3') %>% 
  select(Country, fine.clust, driver.category.2) %>% 
  mutate(obs = rep(1, nrow(.))) %>% 
  pivot_wider(id_cols = c(Country, fine.clust), names_from = driver.category.2, values_from = obs))

seag.dat2[is.na(seag.dat2)] <- 0

# pivot to long format for plotting

plot <- seag.dat2 %>% 
  pivot_longer(-c(Country, fine.clust), names_to = 'driver', values_to = 'presence')

# make a column for continent

plot$Continent <- c(rep('Oceania', 5), rep('Asia', 5), rep('Europe', 15), rep('Asia', 5), rep('Europe', 5), 
                    rep('Asia', 5), rep('Africa', 5), rep('Asia', 5), rep('Africa', 5), rep('Europe', 10),
                    rep('Oceania', 5), rep('Africa', 5), rep('Europe', 15), rep('America', 5), rep('Asia', 5))

# calculate number of countries in each enabling profile and save (will plot manually later)

samps <- plot %>% 
  select(-c(driver, presence)) %>% 
  group_by(Continent, Country, fine.clust) %>% 
  mutate(n = 1) %>% 
  summarise(n = mean(n)) %>% 
  group_by(Continent, fine.clust) %>% 
  summarise(n = sum(n))

write.csv(samps, 'outputs/seagrass-driver-sample-size.csv', row.names = F)

# plot 

plot <- plot %>% 
  select(Continent, fine.clust, driver, presence) %>% 
  distinct() %>% 
  mutate(driver = as.factor(driver))
continent <- unique(plot$Continent)

# loop through df and plot individual pie charts for each continent and enbaling profile,
# showing whether a driver is present or not

for(i in 1:length(continent)){
  for (j in 1:6){
    b <- ggplot(filter(plot, Continent == continent[i] & fine.clust == j)) +
      geom_bar(aes(fill = driver, y = presence, x = Continent), position="stack", stat="identity") +
      scale_fill_manual(values = palette,
                        breaks = c('Aquaculture.and.fishing', 'Boating', 'Catchment.processes', 'Climate.and.storms', 
                                   'Disease'),
                        labels = c('Aqua/Fishing', 'Boating', 'Catchment', 'Climate/Storms', 'Disease')) + 
      ylab('') +
      xlab('') +
      theme_classic() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 9),
            plot.title = element_text(size = 10))
    
    b
    
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank()
      )
    
    b + coord_polar("y", start=0) +
      blank_theme +
      theme(axis.text=element_blank())
    
    ggsave(paste0('outputs/seagrass-drivers', continent[i],'-', j,'.png'), width = 4, height = 3.3)
  }}

# join seag data with country polygons and plot

clust.final <- dplyr::rename(clust.final, iso_a3 = ISO_SOV1)

seag.dat.m <- seag.dat %>% 
  left_join(clust.final, by = 'iso_a3') %>% 
  dplyr::rename('Enabling profile' = fine.clust)

world.clust <- World %>% 
  inner_join(seag.dat.m, by = 'iso_a3') %>% 
  st_transform(crs = 4326) %>% 
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>% 
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

World <- World %>% 
  st_transform(crs = 4326) %>% 
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>% 
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

# plot

tmap_mode('plot')

pal <- brewer.pal(9, 'YlGnBu')
pal <- pal[c(2,3,5,6,9)]

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

tmap_save(m, 'outputs/Fig3.png', width = 8, height = 6)

