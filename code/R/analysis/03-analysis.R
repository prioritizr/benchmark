# restore session
session::restore.session(session_path("02"))

# import parameters
benchmark_parameters <-
  RcppTOML::parseTOML("code/parameters/benchmark.toml")[[MODE]]

# create folder to store raster solutions
dir.create("data/intermediate/solutions", showWarnings = FALSE)

# initialize benchmark results with metadata for each run
benchmark_results <-
  expand.grid(
    pu_data  = seq_along(pu_data_paths),
    number_features = nrow(spp_data),
    relative_target = benchmark_parameters$relative_target,
    boundary_penalty = benchmark_parameters$boundary_penalty_value,
    solver = benchmark_parameters$solver,
    threads = benchmark_parameters$threads,
    time_limit = benchmark_parameters$time_limit,
    gap = benchmark_parameters$gap,
    replicate = seq_len(benchmark_parameters$number_replicates)) %>%
  tibble::as_tibble() %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate(id = seq_len(nrow(.))) %>%
  mutate(number_of_planning_units = pu_data_n[pu_data]) %>%
  select(id, pu_data, number_of_planning_units, everything())

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
    ## create cluster
    cl <- parallel::makeCluster(n_main_thread, type = cl_type)
    ## manually initialize for PSOCK cluster if needed
    ## (FORK clusters don't need this because they share memory with
    ##  the main R process)
    if (identical(cl_type, "PSOCK")) {
      parallel::clusterEvalQ(cl, {library(dplyr)})
      parallel::clusterExport(cl,
        c("spp_data", "pu_data_paths", "bd_data_paths", "pu_raster_data_paths"))
    }
    ## register cluster for plyr
    doParallel::registerDoParallel(cl)
  }
}

# perform benchmark analysis
benchmark_results <-
  benchmark_results %>%
  dplyr::sample_frac() %>% # randomize benchmark order
  plyr::dlply(
    "id",
    .parallel = exists("cl"),
    .progress = ifelse(exists("cl"), "none", "text"),
    function(x) {
    ## print current run
    message("starting run: ", id)
    ## validate arguments
    assertthat::assert_that(nrow(x) == 1)
    ## import planning unit data
    pu <- readRDS(pu_data_paths[[x$pu_data]])
    ## create problem
    p <-
      prioritizr::problem(pu, spp_data$code, cost_column = "cost") %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_relative_targets(x$relative_target) %>%
      prioritizr::add_binary_decisions()
    ## add boundary penalties if needed
    if (x$boundary_penalty > 1e-10) {
      p <-
        p %>%
        prioritizr::add_boundary_penalties(
          x$boundary_penalty,
          data = readRDS(bd_data_paths[[x$pu_data]]))
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
    s <- try(
      prioritizr::solve(p, force = TRUE, run_checks = FALSE),
      silent = TRUE)
    ## if failed to generate solution then create a fake solution object,
    ## to store the results
    if (inherits(s, "try-error")) {
      s <- list()
      attr(s, "objective") <- NA_real_
      attr(s, "status") <- "ERROR"
      attr(s, "runtime") <- NA_real_
    }
    ## create raster with solution
    r <- raster::raster(pu_raster_data_paths[[x$pu_data]])
    if (inherits(s, "data.frame")) {
      ### create raster with solution values if feasible solution found
      r[pu$pu] <- s$solution_1
    } else {
      ### create raster with -1 in all cells to indicate an error
      ### during solving
      r[pu$pu] <- -1
    }
    ## save solution raster
    n <- paste0("data/intermediate/solutions/", x$id, ".tif")
    raster::writeRaster(r, n, overwrite = TRUE, NAflag = TRUE)
    ## prepare outputs
    tibble::tibble(
      id = x$id,
      objective_value = attr(s, "objective")[[1]],
      status = attr(s, "status")[[1]],
      run_time = as.numeric(attr(s, "runtime")[[1]]),
      solution = n)
  }) %>%
  {left_join(x = benchmark_results, by = "id")}

# clean up parallel processing workers
if (general_parameters$threads > 1) {
  cl <- parallel::stopCluster(cl)
  registerDoParallel::stopImplicitCluster(cl)
  rm(cl)
}

# post-processing of benchmark results
benchmark_results <-
  benchmark_results
  select(id, pu_data, number_of_planning_units, everything())

# save session
session::save.session(session_path("03"), compress = "xz")
