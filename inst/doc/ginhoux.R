## ---- echo=FALSE---------------------------------------------------------
set.seed(4)

## ----message=FALSE-------------------------------------------------------
library(SCORPIUS)
data(ginhoux)

## ------------------------------------------------------------------------
ginhoux$expression[1:6, 1:6]

## ------------------------------------------------------------------------
head(ginhoux$sample_info)

## ------------------------------------------------------------------------
expression <- ginhoux$expression
group_name <- ginhoux$sample_info$group_name
dist <- correlation_distance(expression)

## ------------------------------------------------------------------------
dim(dist)
plot(density(dist))

## ------------------------------------------------------------------------
space <- reduce_dimensionality(dist)

## ------------------------------------------------------------------------
draw_trajectory_plot(space)

## ------------------------------------------------------------------------
draw_trajectory_plot(space, progression_group = group_name)

## ------------------------------------------------------------------------
library(ggplot2)
draw_trajectory_plot(space[, c(1, 3)]) + labs(y="Component 3")

## ------------------------------------------------------------------------
filt <- outlier_filter(dist)
expression <- expression[filt, ]
group_name <- group_name[filt]
dist <- dist[filt, filt]
space <- reduce_dimensionality(dist)

## ------------------------------------------------------------------------
draw_trajectory_plot(space[, c(1, 2)])
draw_trajectory_plot(space[, c(1, 3)]) + labs(y = "Component 3")
draw_trajectory_plot(space[, c(2, 3)]) + labs(x = "Component 2", y = "Component 3")

## ------------------------------------------------------------------------
traj <- infer_trajectory(space)

## ------------------------------------------------------------------------
draw_trajectory_plot(space, progression_group = group_name, path = traj$path)

## ----find tafs-----------------------------------------------------------
gimp <- gene_importances(expression, traj$time, num_permutations = 0, num_threads = 8)
gene_sel <- gimp[1:50,]
expr_sel <- expression[,gene_sel$gene]

## ----recalculate time----------------------------------------------------
traj <- infer_trajectory(expr_sel)

## ----visualise tafs------------------------------------------------------
draw_trajectory_heatmap(expr_sel, traj$time, group_name)

## ----moduled tafs--------------------------------------------------------
modules <- extract_modules(scale_quantile(expr_sel), traj$time, verbose = F)
draw_trajectory_heatmap(expr_sel, traj$time, group_name, modules)

