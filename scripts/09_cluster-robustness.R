# assess the robustness of Enabling Profiles
# initial code by CJ Brown, edited by CA Buelow

library(tidyverse)
library(factoextra)
library(rnaturalearth)
library(tmap)
library(sf)
library(randomcoloR)
library(tidyr)
library(vegan)
library(scales)

# data

resid.df <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals_100-mcmc.csv')
data('World')

World <- World %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>%
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')

clusters <- read.csv( 'outputs/cluster.csv')
clust.new <- read.csv('outputs/clus-new-order.csv') %>%
  dplyr::rename(Cluster = cluster)
clusters.final <- clusters %>%
  left_join(clust.new, by = 'Cluster') %>%
  dplyr::rename(iso_a3 = ISO_SOV1)

# add country IDs

resid.df$ID <- clusters.final$iso_a3

# cluster analysis across samples

imcmc <- unique(resid.df$mcmc_sample)
nMCMC <- length(imcmc)
maxc <- 6 #SEt max number of clusters
nclusters <- 6:maxc #set range of clusters you want to run for
this_clust <- which(nclusters == 6)
cat("Will run for this many clusters:",nclusters[this_clust])

# loops over every cluster number, then ever sample and repeats cluster analysis
# may need to update method depending on wat you're doing (uses cluster::pam() below)

dout <- NULL

system.time(for (iclust in this_clust){
  for (isamp in imcmc){
     set.seed(444)
    #Select sample to use for clustering
    irow <- resid.df$mcmc_sample == isamp
    resid.df2 <- resid.df[irow,]
    notinds <- which(names(resid.df2) %in% c("mcmc_sample","ID"))
    #rescale
    max <- min(apply(resid.df2[,-notinds], 2, max))
    min <- max(apply(resid.df2[,-notinds], 2, min))
    resid.df3 <- resid.df2[,-notinds] %>%
      mutate_all(funs(rescale(., to = c(min, max))))
    #do clustering

    df.clust <- cluster::pam(resid.df3, nclusters[iclust], metric = 'euclidean')
    clusters <- data.frame(imcmc = isamp,
                           ID = resid.df2$ID,
                           Cluster = df.clust$cluster, nclust = nclusters[iclust])
    dout <- c(dout, list(clusters))
    print(nclusters[iclust])
  }
}
)

dall <- do.call("rbind", dout)

# convert each clustering for each MCMC sample to a distance matrix format

# IDs
nID <- length(unique(dall$ID))
IDs <- sort(unique(dall$ID)) #sort so we are sure cells are in ascending order

# function that converts dataframe of cluster identities
# to distance matrix representation of clusters

as_dist_mat <- function(i, x, nclust){
  # x is dataframe where rows are IDs and columns are
  # MCMC sample, number or clusters, cell IDs and cluster ID
  # i is character/numeric for the MCMC sample to filter for
  # nclust is numeric for number of clusters.
  dtemp <- dplyr::filter(x, imcmc == i) %>%
    dplyr::filter(nclust == nclust) %>%
    select(ID, Cluster) %>%
    arrange(ID) #arrange so we are certain
  #cells are in ascending order

  dmat <- matrix(0, nrow = nID, ncol = nID)
  for (iclust in 1:nclust){
    dtemp2 <- filter(dtemp, Cluster == iclust)

    #ID pairs - make dataframe where each row represents
    # two cells in the same cluster
    d <- expand.grid(dtemp2$ID, dtemp2$ID)

    #Now match ID to get row/col positions
    d$i <- match(d$Var1, IDs)
    d$j <- match(d$Var2, IDs)

    #convert coordinates to array indices
    d$pos <- d$i + (d$j-1)*nID
    #Now fill out matrix with 1s to represent IDs in same cluster
    dmat[d$pos] <- 1
  }
  dmat
}

# make distance matrices for all MCMC samples for a given cluster size

xout <- lapply(imcmc, as_dist_mat,dall, nclusters[this_clust])
length(xout) == nMCMC

# compare distance matrices to calculate robustness

# for each row (ID) compare it to all other rows across MCMC samples

df_sim <- data.frame(ID = IDs, mn = NA,
                     med = NA,
                     var = NA)

for (iID in 1:nID){
  #get all MCMC samples for one specific cell ID (row)
  #make a matrix of nMCMC x cell IDs
  xout2 <- lapply(xout, function(x) x[iID,]) %>%
    do.call("rbind",.)
  #mean jaccard similarity across samples
  #this compares each sample to every other sample, for a site
  # then we take the mean to get mean similarity
  df_sim$mn[iID] <- mean(apply(xout2, 2, sd))
  df_sim$med[iID] <- median(apply(xout2, 2, sd))
  df_sim$var[iID] <- sd(apply(xout2, 2, sd))
}

# map robustness

brks <- seq(0, 1, by= 0.1)

df_sim <- df_sim %>%
  dplyr::rename(iso_a3 = ID)

world.clust <- World %>%
  left_join(df_sim, by = 'iso_a3') %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -180, ymin = -60, xmax = 180, ymax = 90) %>%
  st_transform(crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs') %>%
  filter(!is.na(mn))

w1 <- tm_shape(world.clust) +
  tm_fill(col = "mn",
          title = "Mean SD")

w2 <- tm_shape(world.clust) +
  tm_fill(col = "med",
          title = "Median SD")

w3 <-  tm_shape(world.clust) +
  tm_fill(col = "var",
          title = "SD of SD")

tm_all <- tmap_arrange(w1, w2, w3)
tm_all
tmap_save(tm_all, filename = "outputs/FigS4.png")

## robustness by typologies

#We can also look at the robustness by specific typologies.
#We take the typologies determined from the cluster analysis of the mean residual values
#(the final results).
#Then for each of those N typologies and the subset of cells
#belonging to that typology we calculate the probability that each pair of cells
#is classified together. We can then summarize this as a distribution, mean, median or SD.

#Here are the distributions of pair membership for each typology

clustsim <- data.frame(Typology = 1:length(unique(clusters.final$Cluster)),
                       Mean = NA,
                       Median = NA,
                       SD = NA)
dcells <- NULL

for(iclust in 1:length(unique(clusters.final$Cluster))){
  dtemp <- filter(clusters.final, fine.clust == iclust)
  IDselect <- IDs %in% dtemp$iso_a3
  xout2 <- lapply(xout, function(x) x[IDselect,IDselect])
  xout3 <- simplify2array(xout2)

  #Probability a pairs of cells are included in the same typo
  xout4 <- as.matrix(apply(xout3, c(1,2), mean))

  #cell wise average pairwise inclusion probability
  dcellstemp <- data.frame(ID = IDs[IDselect],
                           prob = apply(xout4, 2, mean))
  dcells <- c(dcells, list(dcellstemp))

  #Summary stats
  ilower <- lower.tri(xout4)
  clustsim$Mean[iclust] <- mean(xout4[ilower])
  clustsim$Median[iclust]<- median(xout4[ilower])
  clustsim$SD[iclust] <- sd(xout4[ilower])
  print(plot(density(xout4[ilower]),
             xlab = "Pairwise probability",
             main = paste0("Probability density for ",iclust,
                           " typologies")))
}

#Here are the summary values.
#Higher values of the mean and median indicate higher probabilities
#that pairs of cells are included within the same typology.
#Higher SD values indicate higher variation in the pairwise inclusion
#(so some cell pairs had high probability of being included, others a low probability).

clustsim <- clustsim %>%
  tidyr::pivot_longer(-1, names_to = "Statistic",
                      values_to = "Val")

ggplot(clustsim) +
  aes(x = as.factor(Typology), y = Val) +
  geom_bar(stat = "identity") +
  facet_grid(Statistic~., scales = "free") +
  ylab("Statistic") +
  xlab('Enabling profile') +
  theme_bw()

ggsave('outputs/FigS5.png', width = 3, height = 3.5)
