data("tissue_gene_expression")
dim(tissue_gene_expression$x)
str(tissue_gene_expression)

# Q1
pca <- prcomp(tissue_gene_expression$x)
summary(pca)

tissue_types <- sapply(row.names(pca$x), function(x) str_split(x, '_')[[1]][1])

data.frame(pca$x[,1:2], TissueType=tissue_types) %>% 
  ggplot(aes(PC1,PC2,fill=tissue_types)) +
  geom_point(cex=3, pch=21) 

# Q2
obs_means <- rowMeans(tissue_gene_expression$x)
data.frame(PC1=pca$x[,1], ObsMeans=obs_means) %>%
ggplot(aes(PC1, ObsMeans, color=tissue_gene_expression$y)) +
  geom_point(s)
cor(pca$x[,1], obs_means)

# Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4
for(i in 1:10) {
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
#  data.frame(PC = pc$x[,i], tissue=tissue_gene_expression$y) %>%
#    ggplot(aes(PC, tissue)) +
#    geom_boxplot() +
#    labs(title = paste('PC', i))
}

# Q5
var(tissue_gene_expression$x - pc$x[,1])

# Q6
summary(pca)$importance[,1:7] 
plot(summary(pc)$importance[3,])
