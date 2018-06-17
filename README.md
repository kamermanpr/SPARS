# The Sensation and Pain Rating Scale (SPARS)

### Generate PDF copies of supplementary files 

## Information

**Do not merge this branch with _master_**

This branch of the SPARS repo was used to generate the supplementary files referenced in the SPARS mansucript as PDF documents. Minor changes were made to the YAML and the body of _*.Rmd_ scripts to generate well-formatted PDF outputs. The names of these files use the format: _supplement\_\*.pdf_, where _\*_ is an integer numbering the file. 

Auxillary analyses that were not directly referenced as supplementary materials in the mansucript, but which were part of the analysis process, have also been converted tp PDF documents. The names of these files use the format: _experiment\_\*\_details.pdf_, where _\*_ is an integer numbering the experiment (1 or 2), and _details_ provides additional information on the type of analysis contained within the script.

## Bibliometric information

URL for master: [https://github.com/kamermanpr/SPARS](https://github.com/kamermanpr/SPARS)

URL for this branch: [https://github.com/kamermanpr/SPARS/tree/supplementary_pdfs](https://github.com/kamermanpr/SPARS/tree/supplementary_pdfs)

## Build the document

**Requires data from the authors. Participants did not consent to public release of their data, but the data are available on request from the corresponding author listed on the published manuscript (see: _bibliometric information_).**

While the outputs have been provided, you can build the analysis from scratch on your local machine by following the steps below. The analysis was performed on _R version 3.5.0 (2018-04-23) -- "Joy in Playing"_.

Before building the analysis, _Microsoft Windows_ users must first download and install:

- [Git for Windows](https://github.com/git-for-windows/git/releases) or any other Bash-like shell for _MS Windows_ (download the appropriate 32/64-bit *.exe file).

- [GNU Make](http://gnuwin32.sourceforge.net/downlinks/make.php).

**OPTION 1:**

- Fork the repository to your _GitHub_ account, and then clone the repository to your computer.

- Open a terminal and change the path to the root directory of the respository.

- Type _'make clean'_ (removes all outputs and cleaned data).

- Type _'make'_ (builds the analysis).

**OPTION 2:**

- Download the repo as a zip file, and unzip the file.

- Open a terminal and change the path to the root directory of the unzipped folder.

- Type _'make clean'_ (removes all outputs and cleaned data).

- Type _'make'_ (builds the analysis).
