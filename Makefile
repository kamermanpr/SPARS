# Create directories if required
$(shell mkdir -p data-cleaned outputs)

# Dummy outputs
2A = 	outputs/supplement_3.pdf

3A = 	outputs/supplement_4.pdf

4A1 = 	outputs/supplement_5.pdf

4A2 = 	outputs/supplement_6.pdf

4A3 = 	outputs/supplement_7.pdf

4A4 = 	outputs/supplement_8.pdf

4A5 = 	outputs/supplement_9.pdf

4A6 = 	outputs/supplement_10.pdf

1B1 = 	outputs/supplement_11.pdf

1B2 = 	outputs/supplement_12.pdf

2B = 	outputs/supplement_13.pdf

.PHONY: all

all: 	$(2A) $(3A) $(4A1) $(4A2) $(4A3) $(4A4) $(4A5) $(4A6) $(1B1) $(1B2) $(2B)

# Clean
clean:
	rm -rfv outputs/*.*

# Generate outputs
outputs/supplement_3.pdf: \
suppl_03_2A-central-tendency.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_03_2A-central-tendency.pdf outputs/supplement_3.pdf

outputs/supplement_4.pdf: \
suppl_04_3A-order-effects.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_04_3A-order-effects.pdf outputs/supplement_4.pdf

outputs/supplement_5.pdf: \
suppl_05_4A-stimulus-response-1.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_05_4A-stimulus-response-1.pdf outputs/supplement_5.pdf

outputs/supplement_6.pdf: \
suppl_06_4A-stimulus-response-2.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_06_4A-stimulus-response-2.pdf outputs/supplement_6.pdf

outputs/supplement_7.pdf: \
suppl_07_4A-stimulus-response-3.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_07_4A-stimulus-response-3.pdf outputs/supplement_7.pdf

outputs/supplement_8.pdf: \
suppl_08_4A-stimulus-response-4.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_08_4A-stimulus-response-4.pdf outputs/supplement_8.pdf

outputs/supplement_9.pdf: \
suppl_09_4A-stimulus-response-5.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_09_4A-stimulus-response-5.pdf outputs/supplement_9.pdf

outputs/supplement_10.pdf: \
suppl_10_4A-stimulus-response-6.Rmd data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_10_4A-stimulus-response-6.pdf outputs/supplement_10.pdf

outputs/supplement_11.pdf: \
suppl_11_1B-stimulus-response-1.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_11_1B-stimulus-response-1.pdf outputs/supplement_11.pdf

outputs/supplement_12.pdf: \
suppl_12_1B-stimulus-response-2.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_12_1B-stimulus-response-2.pdf outputs/supplement_12.pdf

outputs/supplement_13.pdf: \
suppl_13_2B-scale-agreement.Rmd data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv outputs/suppl_13_2B-scale-agreement.pdf outputs/supplement_13.pdf
