# Create directories if required
$(shell mkdir -p data-cleaned outputs outputs/figures/)

# Dummy outputs
DATA = 	data-cleaned/SPARS_A.csv \
		data-cleaned/SPARS_A.rds

1A = 	outputs/1A-participants-descriptive.md \
		outputs/1A-participants-descriptive.html

2A = 	outputs/2A-central-tendency.md \
		outputs/2A-central-tendency.html

3A = 	outputs/3A-order-effects.md \
		outputs/3A-order-effects.html

.PHONY: all

all: $(DATA) $(1A) $(2A) $(3A)

# Clean
clean:
	rm -r ./outputs ./data-cleaned

# Generate data
data-cleaned/SPARS_A.csv data-cleaned/SPARS_A.rds: \
clean-data.R data/raw-data-18112016-deidentified.xlsx
	Rscript "$<"

# Generate outputs
outputs/1A-participants-descriptive.html outputs/1A-participants-descriptive.md: \
1A-participants-descriptive.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/1A-participants-descriptive outputs/figures/

outputs/2A-central-tendency.html outputs/2A-central-tendency.md: \
2A-central-tendency.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/2A-central-tendency outputs/figures/

outputs/3A-order-effects.html outputs/3A-order-effects.md: \
3A-order-effects.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/3A-order-effects outputs/figures/
	rm -r figures/