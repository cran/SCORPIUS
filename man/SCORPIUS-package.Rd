% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\docType{package}
\name{SCORPIUS-package}
\alias{SCORPIUS-package}
\alias{SCORPIUS}
\title{SCORPIUS: Trajectory inference from single-cell RNA sequencing data.}
\description{
SCORPIUS orders single cells with regard to an implicit timeline,
such as cellular development or progression over time.
}
\section{Dimensionality Reduction functions}{

\code{\link{reduce_dimensionality}}
}

\section{Trajectory Inference functions}{

\code{\link{infer_trajectory}}, \code{\link{infer_initial_trajectory}}, \code{\link{reverse_trajectory}}, \code{\link{gene_importances}}, \code{\link{extract_modules}}
}

\section{Visualisation functions}{

\code{\link{draw_trajectory_plot}}, \code{\link{draw_trajectory_heatmap}}
}

\section{Datasets}{

\code{\link{generate_dataset}}, \code{\link{ginhoux}}
}

\examples{
## Load dataset from Schlitzer et al., 2015
data("ginhoux")

## Reduce dimensionality and infer trajectory with SCORPIUS
space <- reduce_dimensionality(ginhoux$expression, "spearman")
traj <- infer_trajectory(space)

## Visualise
draw_trajectory_plot(
  space,
  path = traj$path,
  progression_group = ginhoux$sample_info$group_name
)
}
\references{
Cannoodt R. et al., SCORPIUS improves trajectory inference and identifies novel modules in dendritic cell development,
bioRxiv (Oct., 2016). \doi{10.1101/079509}
(\href{https://www.biorxiv.org/content/biorxiv/early/2016/10/07/079509.full.pdf}{PDF}).
}
