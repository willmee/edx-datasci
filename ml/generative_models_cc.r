library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# Q1
# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

lda_model <- train(x, y, method="lda")

# Q2
lda_model$finalModel

# from the model solution
t(lda_model$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q3
library(dslabs)      
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

qda_model <- train(x, y, method="qda")

# Q4
t(qda_model$finalModel$means) %>%
  data.frame() %>%
  mutate(gene = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=gene)) +
  geom_point() + 
  geom_text()

# Q5
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

lda_model_centered <- train(x, y, method="lda", preProcess = "center")

t(lda_model_centered$finalModel$means) %>%
  data.frame() %>%
  mutate(gene = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=gene)) +
  geom_point() +
  geom_text()

# Q6
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

lda_model_all <- train(x, y, method="lda")
