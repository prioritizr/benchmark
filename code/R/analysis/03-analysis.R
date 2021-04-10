# restore session
session::restore.session(session_path("02"))

# general_parameters$threads <- 1
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
      msg = "invalid benchmark.toml file")
    assertthat::assert_that(
      is.numeric(x$boundary_penalty_value),
      msg = "invalid benchmark.toml file")
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
      replicate = seq_len(benchmark_parameters$number_replicates))
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
      NA_real_, budget)) %>%
  dplyr::filter(
    !((objective == "add_min_set_objective") & !is.na(budget)))

## print number of rows for logging
message("total number of benchmark runs: ", nrow(benchmark_results))

# prepare cluster for parallel processing
if (general_parameters$threads > 1) {
  ## determine number of threads to use for running benchmark analysis
  ## in parallel
  n_main_thread <-
    floor(general_parameters$threads /  max(benchmark_parameters$threads))
  ## if we can run the benchmark analysis in parallel because
  ## the user isn't trying to fork bomb their computer
  if (n_main_thread > 1) {
    ## determine cluster type
    cl_type <- ifelse(.Platform$OS.type == "unix", "FORK", "PSOCK")
    ## if FORK cluster, then initialize session before creating cluster
    if (identical(cl_type, "FORK")) {
      pu_data <- lapply(pu_data_paths, readRDS)
      bd_data <- lapply(bd_data_paths, readRDS)
      pu_raster_data <- lapply(pu_raster_data_paths, raster::raster)
      total_cost <- lapply(pu_data, function(x) sum(x$cost, na.rm = TRUE))
    }
    ## create cluster
    cl <- parallel::makeCluster(n_main_thread, type = cl_type)
    ## if PSOCK cluster, then initialize it
    if (identical(cl_type, "PSOCK")) {
      parallel::clusterEvalQ(cl, {library(dplyr)})
      parallel::clusterExport(cl,
        c("spp_data", "pu_data_paths", "bd_data_paths", "pu_raster_data_paths"))
      parallel::clusterEvalQ(cl, {
        pu_data <- lapply(pu_data_paths, readRDS)
        bd_data <- lapply(bd_data_paths, readRDS)
        pu_raster_data <- lapply(pu_raster_data_paths, raster::raster)
        total_cost <- lapply(pu_data, function(x) sum(x$cost, na.rm = TRUE))
      })
    }
    ## register cluster for plyr
    doParallel::registerDoParallel(cl)
  }
} else {
  ## initialize session for non-parallel run
  pu_data <- lapply(pu_data_paths, readRDS)
  bd_data <- lapply(bd_data_paths, readRDS)
  pu_raster_data <- lapply(pu_raster_data_paths, raster::raster)
  total_cost <- lapply(pu_data, function(x) sum(x$cost, na.rm = TRUE))
}

## print cluster information for logging
# message("benchmark cluster type: ", cl_type)
# message("number of workers in cluster: ", n_main_thread)

# perform benchmark analysis
benchmark_results <-
  benchmark_results %>%
  # dplyr::sample_frac() %>% # randomize benchmark order
  dplyr::mutate(id2 = seq_len(nrow(.))) %>%
  plyr::ddply(
    "id2",
    .parallel = exists("cl"),
    .progress = ifelse(exists("cl"), "none", "text"),
    function(x) {
    ## print current run
    message("starting run: ", x$id)
    ## validate arguments
    assertthat::assert_that(nrow(x) == 1)
    ## check if run has already been completed and return it if it has
    ## set file paths
    raster_path <- paste0("data/intermediate/solutions/", x$id, ".tif")
    run_path <- paste0("data/intermediate/runs/", x$id, ".rds")
    if (file.exists(run_path) && file.exists(raster_path)) {
      ## try to results if they exist
      out <- try(readRDS(run_path), silent = TRUE)
      ## if both loaded correctly then just return previous result
      if (!inherits(out, "try-error") &&
          !inherits(
            try(raster::raster(raster_path), silent = TRUE),
            "try-error")) {
        out$cache <- TRUE
        return(out)
      }
    }
    ## otherwise, if run has not been completed then run analysis...
    ## create problem
    p <-
      prioritizr::problem(
        pu_data[[x$pu_data]], spp_data$code, cost_column = "cost") %>%
      prioritizr::add_relative_targets(x$relative_target) %>%
      prioritizr::add_binary_decisions()
    ## add objective
    ### find objective
    obj_fun <- try(getFromNamespace(x$objective, "prioritizr"), silent = TRUE)
    if (inherits(obj_fun, "try-error")) {
      stop(paste0(x$solver, " is not a objective in the prioritizr R package"))
    }
    ### prepare objective arguments
    obj_args <- list(x = p, budget = total_cost[[x$pu_data]] * x$budget)
    ### subset arguments to only include supported arguments
    obj_args <-
      obj_args[which(names(obj_args) %in% formalArgs(obj_fun))]
    ### add objective to problem
    p <- do.call(obj_fun, obj_args)
    ## add boundary penalties if needed
    if (x$boundary_penalty > 1e-15) {
      p <-
        p %>%
        prioritizr::add_boundary_penalties(
          x$boundary_penalty,
          data = bd_data[[x$pu_data]])
    }
    ## add solver
    ### find solver
    solver_fun <- try(getFromNamespace(x$solver, "prioritizr"), silent = TRUE)
    if (inherits(solver_fun, "try-error")) {
      stop(paste0(x$solver, " is not a solver in the prioritizr R package"))
    }
    ### prepare solver arguments
    solver_args <-
      list(x = p, threads = x$threads, time_limit = x$time_limit, gap = x$gap,
           verbose = FALSE)
    ### subset arguments to only include supported arguments
    solver_args <-
      solver_args[which(names(solver_args) %in% formalArgs(solver_fun))]
    ### add solver to problem
    p <- do.call(solver_fun, solver_args)
    ## generate solution
    total_time <- system.time({
      s <- try(
        prioritizr::solve(p, force = TRUE, run_checks = FALSE),
        silent = TRUE)
    })[[3]]
    ## free memory
    rm(p, solver_args, solver_fun); gc();
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
    r <- pu_raster_data[[x$pu_data]]
    if (inherits(s, "data.frame")) {
      ### create raster with solution values if feasible solution found
      r[pu_data[[x$pu_data]]$pu] <- s$solution_1
    } else {
      ### create raster with -1 in all cells to indicate an error
      ### during solving
      r[pu_data[[x$pu_data]]$pu] <- -1
    }
    ## save solution raster
    raster::writeRaster(r, raster_path, overwrite = TRUE, NAflag = -9999)
    ## free memory
    rm(s, r); gc();
    ## prepare outputs
    out <-
      tibble::tibble(
        id = x$id,
        objective_value = s_objective,
        status = s_status,
        total_time = as.numeric(s_total_time),
        run_time = as.numeric(s_solver_time),
        exceeded_run_time = s_solver_time > (x$time_limit + 1),
        solution = basename(raster_path),
        cache = FALSE)
    ## save output
    saveRDS(out, run_path, compress = "xz")
    ## return result
    out
  }) %>%
  left_join(x = benchmark_results, by = "id")

# clean up parallel processing workers
if (general_parameters$threads > 1) {
  cl <- parallel::stopCluster(cl)
  doParallel::stopImplicitCluster()
  rm(cl)
}

# clean up
rm(pu_data, pu_raster_data, bd_data)

# save session
session::save.session(session_path("03"), compress = FALSE)
