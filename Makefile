## variables
# MODE=release# set parameters for inference
MODE=debug# set parameters for debugging code

## main operations
R:
	R --quiet --no-save

all: install raw_data analysis push_results

clean:
	@rm -rf data/intermediate/*
	@rm -rf data/final/*
	@touch data/intermediate/.gitkeep
	@touch data/final/.gitkeep
	@touch article/.gitkeep

# commands for updating time-stamps
touch:
	touch data/intermediate/00*.rda
	touch data/intermediate/01*.rda
	touch data/intermediate/02*.rda
	touch data/intermediate/03*.rda

# commands for running analysis
analysis: data/final/results.rda

data/final/results.rda: data/intermediate/03-*.rda code/R/analysis/04-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/04-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/03-*.rda: data/intermediate/02-*.rda code/R/analysis/03-*.R code/parameters/benchmark.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/03-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/02-*.rda: data/intermediate/01-*.rda code/R/analysis/02-*.R code/parameters/data.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/02-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/01-*.rda: data/intermediate/00-*.rda code/R/analysis/01-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/01-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/00-*.rda: code/R/analysis/00-*.R code/parameters/general.toml code/R/functions/misc.R code/R/functions/session_path.R
	R CMD BATCH --no-restore --no-save '--args MODE=$(MODE)' code/R/analysis/00-*.R
	mv -f *.Rout data/intermediate/

# command to install dependencies
install:
	R CMD BATCH --no-restore --no-save '--args --bootstrap-packrat' packrat/init.R
	mv -f *.Rout data/intermediate/

# packrat commands
packrat_update_local_packages:
	R -e "packrat::install('../prioritizr', build_vignettes = FALSE)"

packrat_snapshot:
	R -e "packrat::snapshot(infer = FALSE)"

# command to download data
raw_data: data/raw/planning-units/nplcc_cost_occupancy.zip

data/raw/planning-units/nplcc_cost_occupancy.zip:
	R -e "piggyback::pb_download('nplcc_cost_occupancy.zip',repo='prioritizr/benchmark',dest='data/raw/planning-units',tag='v0.0.1')"

.PHONY: install raw_data analysis push_results
