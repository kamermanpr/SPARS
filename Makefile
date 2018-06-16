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

4A1 = 	outputs/4A-stimulus-response-1.md \
		outputs/4A-stimulus-response-1.html

4A2 = 	outputs/4A-stimulus-response-2.md \
		outputs/4A-stimulus-response-2.html

4A3 = 	outputs/4A-stimulus-response-3.md \
		outputs/4A-stimulus-response-3.html

4A4 = 	outputs/4A-stimulus-response-4.md \
		outputs/4A-stimulus-response-4.html

4A5 = 	outputs/4A-stimulus-response-5.md \
		outputs/4A-stimulus-response-5.html

4A6 = 	outputs/4A-stimulus-response-6.md \
		outputs/4A-stimulus-response-6.html

1B1 = 	outputs/1B-stimulus-response-1.md \
		outputs/1B-stimulus-response-1.html

1B2 = 	outputs/1B-stimulus-response-2.md \
		outputs/1B-stimulus-response-2.html

2B = 	outputs/2B-scale-agreement.md \
		outputs/2B-scale-agreement.html

.PHONY: all

all: 	$(DATA_A) $(DATA_B) $(1A) $(2A) $(3A) \
		$(4A1) $(4A2) $(4A3) $(4A4) $(4A5) $(4A6) \
		$(5B1) $(5B2) $(6B)

# Clean
clean:
	rm -r ./outputs ./data-cleaned

# Generate data
data-cleaned/SPARS_A.csv data-cleaned/SPARS_A.rds: \
0A-clean-data.R data/*.xlsx
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

outputs/4A-stimulus-response-1.html outputs/4A-stimulus-response-1.md: \
4A-stimulus-response-1.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-stimulus-response-1 outputs/figures/

outputs/4A-stimulus-response-2.html outputs/4A-stimulus-response-2.md: \
4A-stimulus-response-2.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-stimulus-response-2 outputs/figures/

outputs/4A-stimulus-response-3.html outputs/4A-stimulus-response-3.md: \
4A-stimulus-response-3.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-stimulus-response-3 outputs/figures/

outputs/4A-stimulus-response-4.html outputs/4A-stimulus-response-4.md: \
4A-stimulus-response-4.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-stimulus-response-4 outputs/figures/

outputs/4A-stimulus-response-5.html outputs/4A-stimulus-response-5.md: \
4A-stimulus-response-5.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-stimulus-response-5 outputs/figures/

outputs/4A-stimulus-response-6.html outputs/4A-stimulus-response-6.md: \
4A-stimulus-response-6.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/4A-stimulus-response-6 outputs/figures/

outputs/1B-stimulus-response-1.html outputs/1B-stimulus-response-1.md: \
1B-stimulus-response-1.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/1B-stimulus-response-1 outputs/figures/

outputs/1B-stimulus-response-2.html outputs/1B-stimulus-response-2.md: \
1B-stimulus-response-2.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/1B-stimulus-response-2 outputs/figures/

outputs/2B-scale-agreement.html outputs/2B-scale-agreement.md: \
2B-scale-agreement.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/2B-scale-agreement outputs/figures/
