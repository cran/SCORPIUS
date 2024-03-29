#' Run scorpius using the dynwrap pipeline
#'
#' @param expression Expression matrix
#' @param parameters Parameters
#' @param priors Priors
#' @param verbose Verbosity level
#' @param seed Random seed
#' 
#' @noRd
ti_scorpius_run_fun <- function(expression, priors, parameters, seed = NULL, verbose = 0)  {
  if (!requireNamespace("dynwrap", quietly = TRUE)) {
    stop("install dynwrap in order to use ti_scorpius().")
  } else {
    if (!is.null(seed)) set.seed(seed)

    # TIMING: done with preproc
    checkpoints <- list(method_afterpreproc = Sys.time())

    # REDUCE DIMENSIONALITY
    # use prior dimred if available
    space <-
      if (is.null(priors$dimred)) {
        SCORPIUS::reduce_dimensionality(
          x = expression,
          dist = parameters$distance_method,
          ndim = parameters$ndim
        )
      } else {
        priors$dimred
      }

    # INFER TRAJECTORY
    traj <- infer_trajectory(
      space,
      k = parameters$k,
      thresh = parameters$thresh,
      maxit = parameters$maxit,
      stretch = parameters$stretch,
      smoother = parameters$smoother,
      approx_points = 100
    )

    # TIMING: done with method
    checkpoints$method_aftermethod <- Sys.time()

    # SAVE OUTPUT
    output <-
      dynwrap::wrap_data(
        cell_ids = names(traj$time)
      ) %>%
      dynwrap::add_linear_trajectory(
        pseudotime = traj$time
      ) %>%
      dynwrap::add_timings(
        timings = checkpoints
      )

    # convert trajectory to segments
    dimred_segment_points <- traj$path
    dimred_segment_progressions <- output$progressions %>% select("from", "to", "percentage")

    output <-
      output %>%
      dynwrap::add_dimred(
        dimred = space,
        dimred_segment_points = dimred_segment_points,
        dimred_segment_progressions = dimred_segment_progressions,
        connect_segments = TRUE
      )

    output
  }
}

#' Infer a trajectory using SCORPIUS
#'
#' Pass this object to [dynwrap::infer_trajectory()].
#'
#' @eval dynwrap::generate_parameter_documentation(ti_scorpius())
#' 
#' @return A dynwrap TI method.
#'
#' @importFrom dynwrap create_ti_method_r
#' @export
ti_scorpius <- dynwrap::create_ti_method_r(
  definition = system.file("dynwrap/definition.yml", package = "SCORPIUS"),
  run_fun = ti_scorpius_run_fun
)
