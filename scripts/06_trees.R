# determine the importance of indicators for classifying countries into Enabling Profiles with classification trees

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(reshape)
library(tidyverse)
library(patchwork)
library(viridis)
library(scales)

# data

clustdat <- read.csv('outputs/cluster.csv')
inddat <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final.csv')
rawind <- read.csv('data/master-df_final.csv') %>%
  select(ISO_SOV1, WGI_GovEff_est:mean_annual_funds)
clust.new <- read.csv('outputs/clus-new-order.csv') %>% 
  dplyr::rename(Cluster = cluster)
labels <- read.csv('data/indicator-labels.csv',  fileEncoding = "UTF-8-BOM") %>% 
  dplyr::rename(variable = indicator)

# rescale

max <- min(apply(inddat, 2, max))
min <- max(apply(inddat, 2, min))

inddat <- inddat %>% 
  mutate_all(funs(rescale(., to = c(min, max))))

# join country names to indicator data

inddat$ISO_SOV1 <- rawind$ISO_SOV1

dtemp <- clustdat %>% 
  right_join(inddat, by = 'ISO_SOV1')

dtemp <- dtemp %>% 
  left_join(clust.new, by = 'Cluster') %>% 
  distinct()

# select variables needed for classification tree

dtemp2 <- select(dtemp, OECD_sc:ramsar.manage, fine.clust)

set.seed(555)

##### full tree #####

# note: because goal is to determine importance of variables for classifying clusters,
# and goal is not to build a predictive model generalisable to novel data/conditions
# we do not parition the data into training and testing datasets,
# but rather use the full dataset to determine variable importance/classification rules

tree <- rpart(fine.clust ~ ., data = dtemp2, method = "class")

# variable importance
# An overall measure of variable importance is the sum of the goodness of split measures for each split for 
# which it was the primary variable, plus goodness * (adjusted agreement) for all splits in which it was a surrogate. 
# In the printout these are scaled to sum to 100 and the rounded values are shown, omitting any variable whose proportion 
# is less than 1%.

df <- data.frame(imp = tree$variable.importance)

# add variables <1%

a <- rownames(df)
b <- colnames(dtemp)[-length(dtemp)]
c <- b[!(b %in% a)]
d <- data.frame(imp = rep(0.5, length(c)))
rownames(d) <- c
df <- rbind(df, d)

# dataframe for plotting

df3 <- df %>% 
  rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  left_join(labels, by = 'variable') %>% 
  arrange(imp) %>%
  filter(!is.na(label)) %>% 
  droplevels() %>% 
  mutate(label = forcats::fct_inorder(label))
  
df3$Category <- factor(df3$Category, levels = c('Policy', 'Regulation', 'Engagement'))

cbbPalette <- c('azure3', 'azure4', 'black') # color palette

a <- ggplot(df3) +
  geom_col(aes(x = label, y = imp, 
               fill = Category), 
           alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(values = cbbPalette) +
  xlab('') +
  ylab('Indicator importance') +
  ggtitle('B)') +
  theme_classic() +
  theme(legend.position = c(0.7, 0.2),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10))
a # will plot later with next figure below

##### tree for each indicator to determine classification thresholds #####

# only fit to interquartile range of data to avoid outlier leverage

# loop to fit individual trees, store ouput in list (ls)

iterate <- length(dtemp2)-1
ls <- list()

for(i in 1:iterate){
  clust <- length(dtemp2)
  ind <- dtemp2[,c(i,clust)]
  name <- colnames(ind)[1]
  ggplot(ind) + geom_boxplot(aes_string(x = 'fine.clust', y = paste0(name), group = 'fine.clust'))
  ggsave(paste0('outputs/cluster-boxplots/', name, '.png'), width = 5, height = 2)
  ind.upp <- data.frame(ind %>% group_by(fine.clust) %>% summarise_at(vars(name), ~quantile(., probs = 0.75)))
  ind.low <- data.frame(ind %>% group_by(fine.clust) %>% summarise_at(vars(name), ~quantile(., probs = 0.25)))
  ind.wide <- data.frame(ind %>% arrange(fine.clust) %>% mutate(row = row_number()) %>% pivot_wider(names_from = 'fine.clust', values_from = paste0(name)))
  ind.wide1 <- ind.wide[,2][ind.wide[,2] < ind.upp[1,2] & ind.wide[,2] > ind.low[1,2]]
  ind.wide2 <- ind.wide[,3][ind.wide[,3] < ind.upp[2,2] & ind.wide[,3] > ind.low[2,2]]
  ind.wide3 <- ind.wide[,4][ind.wide[,4] < ind.upp[3,2] & ind.wide[,4] > ind.low[3,2]]
  ind.wide4 <- ind.wide[,5][ind.wide[,5] < ind.upp[4,2] & ind.wide[,5] > ind.low[4,2]]
  ind.wide5 <- ind.wide[,6][ind.wide[,6] < ind.upp[5,2] & ind.wide[,6] > ind.low[5,2]]
  ind.wide6 <- ind.wide[,7][ind.wide[,7] < ind.upp[6,2] & ind.wide[,7] > ind.low[6,2]]
  ind.wide.all <- c(ind.wide1, ind.wide2, ind.wide3, ind.wide4, ind.wide5, ind.wide6)
  ind.clust <- c(rep(1, length(ind.wide1)), rep(2, length(ind.wide2)), rep(3, length(ind.wide3)), rep(4, length(ind.wide4)), rep(5, length(ind.wide5)),
                 rep(6, length(ind.wide6)))
  ind.df <- data.frame(indicator = ind.wide.all, fine.clust = ind.clust)
  ind.df <- ind.df[complete.cases(ind.df),]
  colnames(ind.df) <- c(paste0(name), 'fine.clust')
  tree <- rpart(fine.clust ~ ., data = ind.df, method = "class",
                control = rpart.control(minsplit = 0, minbucket = 0))
  df <- data.frame(rpart.rules(tree))
  ls[[i]] <- df[,c(1,4:ncol(df))]
}

# extract classification decision rules from each tree
# if clusters have two splitting rules, use the highest one

ls2 <- list()

for(i in seq_along(ls)){
  df <- ls[[i]]
  df[,ncol(df)] <- as.numeric(df[,ncol(df)])
  if(length(unique(df$fine.clust)) == nrow(df)){
    ls2[[i]] <- df
  }else{
    dup <- filter(df, duplicated(fine.clust))$fine.clust
    if(length(dup) >= 2){
      vals <- c()
      for(j in 1:length(dup)){
        filt <- filter(df, fine.clust == dup[j])
        vals[j] <- min(filt[,ncol(df)])
      }
      ls2[[i]] <- df[!df[,ncol(df)] %in% vals,]
    }else{
      filt <- filter(df, fine.clust == dup)
      val <- min(filt[,ncol(df)], na.rm = T)
      ls2[[i]] <- df[df[,ncol(df)] != val,]
    }}
}

ls3 <- list()

for(i in seq_along(ls2)){
  df <- ls2[[i]]
  if(ncol(df) > 5){
    min <- min(dtemp2[,i])
    max <- max(dtemp2[,i])
    ls3[[i]] <- mutate(df, Var.6 = ifelse(Var.5 == '< ', min, Var.6),
                       Var.6 = ifelse(Var.5 == '>=', Var.8, Var.6))}
  else{
    df2 <- data.frame(df, Var.7 = 'binary', Var.8 = df$Var.6)
    min <- min(dtemp2[,i])
    max <- max(dtemp2[,i])
    ls3[[i]] <- mutate(df2, Var.6 = ifelse(Var.5 == '< ', min, Var.6),
                       Var.6 = ifelse(Var.5 == '>=', Var.8, Var.6))}
}

ls4 <- list()

for(i in seq_along(ls3)){
  df <- ls3[[i]]
  if(length(duplicated(df$fine.clust)[duplicated(df$fine.clust) ==TRUE]) >= 1){
    df2<- df[!duplicated(df$fine.clust),]
    ls4[[i]] <- df2}
  else{
    ls4[[i]] <- df
  }
}

rules <- do.call(rbind, ls4)

colnames(rules) <- c('Cluster', 'Indicator', 'rule1', 'val1', 'rule2', 'val2')
rules$val1 <- as.numeric(rules$val1)
rules$val2 <- as.numeric(rules$val2)
rules$Indicator <- as.factor(rules$Indicator)
rules <- filter(rules, !is.na(val1))

# sort clusters from high to low on each indicators decison rules

rules.sort <- rules %>%
  group_by(Indicator) %>% 
  arrange(val1, .by_group = T) %>% 
  arrange(val2, .by_group = T)

# assign color palette from low to high and plot as heatmap

range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

rules$Indicator <- as.character(rules$Indicator)

rules.sub <- rules %>% 
  select(Cluster, Indicator, val1)

rules.wide <- rules.sub %>% 
  pivot_wider(names_from = Indicator, values_from = val1) 
rules.wide2 <- data.frame(apply(rules.wide[2:ncol(rules.wide)], 2, range01))
rules.wide2$Cluster <- as.factor(rules.wide$Cluster)
datmap <- rules.wide2

# plot heatmap

lev <- levels(as.factor(df3$label))
labs <- select(df3, variable, label)
dat.m <- melt(as.data.frame(datmap), id = 'Cluster') %>% 
  left_join(labs, by = 'variable')
dat.m$label <- factor(dat.m$label, levels = lev)

b <- ggplot(dat.m, aes(y = label, x = Cluster)) +
  geom_tile(aes(fill = value), colour = "white") +
  xlab('Enabling profile') +
  ylab('') +
  ggtitle('C)') +
  scale_fill_distiller(palette = "BuPu", direction = 1) +
  theme(axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, 'cm'),
    plot.title = element_text(size = 10))
b

f <- a + b
f

ggsave('outputs/Fig1AB.png', width = 6.5, height = 4)
