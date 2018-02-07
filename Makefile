# Create directories if required
$(shell mkdir -p data-cleaned outputs outputs/figures/)

# Dummy outputs
DATA_A = 	data-cleaned/SPARS_A.csv \
			data-cleaned/SPARS_A.rds

DATA_B = 	data-cleaned/SPARS_B.csv \
			data-cleaned/SPARS_B.rds

1A = 	outputs/1A-participants-descriptive.md \
		outputs/1A-participants-descriptive.html

2A = 	outputs/2A-central-tendency.md \
		outputs/2A-central-tendency.html

3A = 	outputs/3A-order-effects.md \
		outputs/3A-order-effects.html

4A = 	outputs/4A-response-characteristics.md \
		outputs/4A-response-characteristics.html

4B = 	outputs/4B-response-characteristics.md \
		outputs/4B-response-characteristics.html

.PHONY: all

all: $(DATA_A) $(DATA_B) $(1A) $(2A) $(3A) $(4A) $(4B)

# Clean
clean:
	rm -r ./outputs ./data-cleaned

# Generate data
data-cleaned/SPARS_A.csv data-cleaned/SPARS_A.rds: \
0A-clean-data.R data/raw-data-18112016-deidentified.xlsx
	Rscript "$<"

data-cleaned/SPARS_B.csv data-cleaned/SPARS_B.rds: \
0B-clean-data.R data/*.txt
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

outputs/4A-response-characteristics.html outputs/4A-response-characteristics.md: \
4A-response-characteristics.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-response-characteristics outputs/figures/

outputs/4B-response-characteristics.html outputs/4B-response-characteristics.md: \
4B-response-characteristics.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4B-response-characteristics outputs/figures/
	rm -r figures/
