## variables
# MODE=debug# set parameters for debugging code
MODE=release# set parameters for inference

## main operations
R:
	R --quiet --no-save

all: install raw_data analysis

all_and_export: all export

clean:
	@rm -rf data/intermediate/*
	@rm -rf data/final/*
	@rm -rf results/*
	@touch data/intermediate/.gitkeep
	@touch data/final/.gitkeep
	@touch results/.gitkeep

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
	export R_PROGRESSR_ENABLE=TRUE && R CMD BATCH --no-restore --no-save code/R/analysis/03-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/02-*.rda: data/intermediate/01-*.rda code/R/analysis/02-*.R code/parameters/data.toml
	R CMD BATCH --no-restore --no-save code/R/analysis/02-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/01-*.rda: data/intermediate/00-*.rda code/R/analysis/01-*.R
	R CMD BATCH --no-restore --no-save code/R/analysis/01-*.R
	mv -f *.Rout data/intermediate/

data/intermediate/00-*.rda: code/R/analysis/00-*.R code/parameters/general.toml code/R/functions/session.R
	R CMD BATCH --no-restore --no-save '--args MODE=$(MODE)' code/R/analysis/00-*.R
	mv -f *.Rout data/intermediate/

# command to install dependencies
install:
	R CMD BATCH --no-restore --no-save code/R/scripts/init.R
	mv -f *.Rout data/intermediate/

snapshot:
	R -e "renv::snapshot()"

# command to make readme
readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

# command to download data
raw_data: data/raw/planning-units/nplcc_cost_occupancy.zip

data/raw/planning-units/nplcc_cost_occupancy.zip:
	R -e "piggyback::pb_download('nplcc_cost_occupancy.zip',repo='prioritizr/benchmark',dest='data/raw/planning-units',tag='v0.0.1')"

# command to export data so it can be accessed by prioritizr vignette
export: results/results.rda results/solutions.zip
	R -e "piggyback::pb_upload('results/results.rda',repo='prioritizr/benchmark',tag='v0.0.6')"
	R -e "piggyback::pb_upload('results/solutions.zip',repo='prioritizr/benchmark',tag='v0.0.6')"

.PHONY: install raw_data analysis export
