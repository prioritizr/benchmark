# restore session
restore_session("02")

# import parameters
benchmark_parameters <-
  RcppTOML::parseTOML("code/parameters/benchmark.toml")[[MODE]]

# create folder to store results
dir.create("data/intermediate/runs", showWarnings = FALSE)
dir.create("data/intermediate/solutions", showWarnings = FALSE)

# initialize benchmark results with metadata for each run
## create data
benchmark_results <-
  plyr::ldply(benchmark_parameters$objective, function(x) {
    # validate parameters
    assertthat::assert_that(
      is.character(x$name),
      msg = "invalid benchmark.toml file"
    )
    assertthat::assert_that(
      is.numeric(x$boundary_penalty_value),
      msg = "invalid benchmark.toml file"
    )
    # generate parameters
    expand.grid(
      pu_data  = seq_along(pu_data_paths),
      number_features = nrow(spp_data),
      objective = x$name,
      budget = benchmark_parameters$budget,
      relative_target = benchmark_parameters$relative_target,
      boundary_penalty = x$boundary_penalty_value,
      solver = benchmark_parameters$solver,
      threads = benchmark_parameters$threads,
      time_limit = benchmark_parameters$time_limit,
      gap = benchmark_parameters$gap,
      replicate = seq_len(benchmark_parameters$number_replicates)
    )
  }) %>%
  tibble::as_tibble() %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate(number_of_planning_units = pu_data_n[pu_data]) %>%
  mutate(id = vapply(
    seq_len(nrow(.)), FUN.VALUE = character(1), function(i) {
      digest::digest(.[i, ])
  })) %>%
  select(id, pu_data, number_of_planning_units, everything())

## manually set budget to NA for min set objective and remove duplicates
benchmark_results <-
  benchmark_results %>%
  dplyr::mutate(
    budget = dplyr::if_else(
      (objective == "add_min_set_objective") & (budget == first(budget)),
      NA_real_,
      budget
    )
  ) %>%
  dplyr::filter(
    !((objective == "add_min_set_objective") & !is.na(budget))
  )

## print number of rows for logging
message("total number of benchmark runs: ", nrow(benchmark_results))

# add columns to benchmark data
benchmark_results <-
  benchmark_results %>%
  # add file paths to the benchmark data
  dplyr::mutate(
    raster_path = paste0("data/intermediate/solutions/", id, ".tif"),
    run_path = paste0("data/intermediate/runs/", id, ".rds")
  ) %>%
  # find out which runs have already been processed
  ## first pass is to see if output files actually exists...
  dplyr::mutate(
    cache = file.exists(raster_path) & file.exists(run_path)
  ) %>%
  ## second pass is to see if the output files contain meaningful data...
  plyr::dlply("id", function(x) {
    if (!x$cache) return(x)
    valid_raster <- !inherits(
      try(terra::rast(x$raster_path), silent = TRUE),
      "try-error"
    )
    valid_run <- !inherits(
      try(readRDS(x$run_path), silent = TRUE),
      "try-error"
    )
    x$cache <- valid_raster && valid_run
    x
  }) %>%
  dplyr::bind_rows()

## print number of rows that have not already been processed
message(
  "total number of remaining benchmark runs: ",
  sum(benchmark_results$cache)
)

# perform benchmark analysis
## set cluster
if (general_parameters$threads > 1) {
  future::plan("multisession", workers = general_parameters$threads)
} else {
  future::plan("sequential")
}

# define benchmark function
bench_fun <- function(id) {
    ## print current run
    message("starting run: ", id)
    ## find metedata for problem
    x <- benchmark_results[benchmark_results$id == id, , drop = FALSE]
    ## import data
    curr_pu_data <- readRDS(pu_data_paths[x$pu_data])
    curr_bd_data <- readRDS(bd_data_paths[x$pu_data])
    ## validate arguments
    assertthat::assert_that(nrow(x) == 1)
    ## otherwise, if run has not been completed then run analysis...
    ## create problem
    p <-
      prioritizr::problem(
        curr_pu_data, spp_data$code, cost_column = "cost"
      ) %>%
      prioritizr::add_relative_targets(x$relative_target) %>%
      prioritizr::add_binary_decisions()
    ## add objective
    ### find objective
    obj_fun <- try(getFromNamespace(x$objective, "prioritizr"), silent = TRUE)
    if (inherits(obj_fun, "try-error")) {
      stop(
        paste0(
          "`", x$objective,
          "` is not an objective in the prioritizr R package"
        )
      )
    }
    ### prepare objective arguments
    obj_args <- list(x = p, budget = pu_total_cost[[x$pu_data]] * x$budget)
    ### subset arguments to only include supported arguments
    obj_args <- obj_args[which(names(obj_args) %in% formalArgs(obj_fun))]
    ### add objective to problem
    p <- do.call(obj_fun, obj_args)
    ## add boundary penalties if needed
    if (x$boundary_penalty > 1e-15) {
      p <- prioritizr::add_boundary_penalties(p,
        x$boundary_penalty,
        data = curr_bd_data
      )
    }
    ## add solver
    ### find solver
    solver_fun <- try(getFromNamespace(x$solver, "prioritizr"), silent = TRUE)
    if (inherits(solver_fun, "try-error")) {
      stop(
        paste0("`", x$solver, "` is not a solver in the prioritizr R package")
      )
    }
    ### prepare solver arguments
    solver_args <- list(
      x = p, threads = x$threads, time_limit = x$time_limit, gap = x$gap,
      verbose = TRUE
    )
    ### subset arguments to only include supported arguments
    solver_args <-
      solver_args[which(names(solver_args) %in% formalArgs(solver_fun))]
    ### add solver to problem
    p <- do.call(solver_fun, solver_args)
    ## generate solution
    total_time <- system.time({
      s <- try(
        solve(p, force = TRUE, run_checks = FALSE),
        silent = TRUE
      )
    })[[3]]
    ## free memory
    rm(p, solver_args, solver_fun)
    gc()
    ## extract results
    if (inherits(s, "try-error")) {
      s_objective <- NA_real_
      s_status <- "ERROR"
      s_solver_time <-  NA_real_
      s_total_time <- NA_real_
    } else {
      s_objective <- attr(s, "objective")[[1]]
      s_status <- attr(s, "status")[[1]]
      s_solver_time <- as.numeric(unname(attr(s, "runtime")[[1]]))
      s_total_time <- total_time
      s <- dplyr::select(s, "solution_1")
    }
    ## create raster with solution
    r <- terra::rast(pu_raster_data_paths[[x$pu_data]])
    if (inherits(s, "data.frame")) {
      ### create raster with solution values if feasible solution found
      r[curr_pu_data$pu] <- s$solution_1
    } else {
      ### create raster with -1 in all cells to indicate an error
      ### during solving
      r[curr_pu_data$pu] <- -1
    }
    ## save solution raster
    terra::writeRaster(r, x$raster_path, overwrite = TRUE, NAflag = -9999)
    ## free memory
    rm(s, r)
    gc()
    ## prepare outputs
    out <- tibble::tibble(
      id = x$id,
      objective_value = s_objective,
      status = s_status,
      total_time = as.numeric(s_total_time),
      run_time = as.numeric(s_solver_time),
      exceeded_run_time = s_solver_time > (x$time_limit + 1),
      solution = basename(x$raster_path)
    )
    ## save output
    saveRDS(out, x$run_path, compress = "xz")
    ## increment progress bar
    pb()
    ## return success
    TRUE
}

## do runs without lpsymphony with optional parallel processing
## to avoid fork bombs
benchmark_run_1 <-
  benchmark_results %>%
  dplyr::filter(!cache) %>%
  dplyr::filter(solver != "add_lpsymphony_solver")
results_part_1 <- progressr::with_progress({
  pb <- progressr::progressor(nrow(benchmark_run_1))
  results <- future.apply::future_lapply(
    benchmark_run_1$id,
    future.seed = NULL,
    FUN = bench_fun
  )
})

## set single-threaded processing
future::plan("sequential")

## do runs for lpsymphony separately
if ("add_lpsymphony_solver" %in% benchmark_results$solver) {
  benchmark_run_2 <-
    benchmark_results %>%
    dplyr::filter(!cache) %>%
    dplyr::filter(solver == "add_lpsymphony_solver")
  results_part_2 <- progressr::with_progress({
    pb <- progressr::progressor(nrow(benchmark_run_2))
    results <- future.apply::future_lapply(
      benchmark_run_2$id,
      future.seed = NULL,
      FUN = bench_fun
    )
  })
}

## merge runs together
if ("add_lpsymphony_solver" %in% benchmark_results$solver) {
  results <- append(results_part_1, results_part_2)
} else {
  results <- results_part_1
}

## verify that everything worked
assertthat::assert_that(
  all(unlist(results, recursive = TRUE, use.names = FALSE)),
  msg = "some benchmark runs failed, try debugging this manually..."
)

# import results
benchmark_results <-
  benchmark_results %>%
  dplyr::left_join(
    dplyr::bind_rows(lapply(benchmark_results$run_path, readRDS)),
    by = "id"
  ) %>%
  tibble::as_tibble()

# clean up
rm(
  pu_data, pu_raster_data, bd_data, results,
  benchmark_run_1, benchmark_run_2,
  results_part_1, results_part_2
)

# save session
save_session("03")
