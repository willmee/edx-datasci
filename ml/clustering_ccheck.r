data("tissue_gene_expression")
dim(tissue_gene_expression$x)
str(tissue_gene_expression)


# Q1
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

# Q2
h <- hclust(d)
plot(h, labels=tissue_gene_expression$y)

# Q3
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

#BLANK
