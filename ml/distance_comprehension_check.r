library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
d <- dist(tissue_gene_expression$x)

# Q2
sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[2,]))
sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[39,]))
sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[40,]))
sqrt(crossprod(tissue_gene_expression$x[2,] - tissue_gene_expression$x[39,]))
sqrt(crossprod(tissue_gene_expression$x[2,] - tissue_gene_expression$x[40,]))

sqrt(crossprod(tissue_gene_expression$x[39,] - tissue_gene_expression$x[40,]))
sqrt(crossprod(tissue_gene_expression$x[39,] - tissue_gene_expression$x[73,]))
sqrt(crossprod(tissue_gene_expression$x[39,] - tissue_gene_expression$x[74,]))
sqrt(crossprod(tissue_gene_expression$x[40,] - tissue_gene_expression$x[73,]))
sqrt(crossprod(tissue_gene_expression$x[40,] - tissue_gene_expression$x[74,]))

sqrt(crossprod(tissue_gene_expression$x[73,] - tissue_gene_expression$x[74,]))
sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[73,]))
sqrt(crossprod(tissue_gene_expression$x[2,] - tissue_gene_expression$x[73,]))
sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[74,]))
sqrt(crossprod(tissue_gene_expression$x[2,] - tissue_gene_expression$x[74,]))

mean(as.vector(d))

# Q3
image(as.matrix(d))