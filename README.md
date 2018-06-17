# The Sensation and Pain Rating Scale (SPARS)

## Bibliometric information


## Abstract

In experiments on pain perception, participants are frequently exposed to non-painful and painful stimuli, yet the conventional pain-rating scales lack a non-painful range and a clear point of transition from non-painful to painful events. The Sensation and Pain Rating Scale (SPARS) is a 0-100 scale that assesses the full stimulus intensity range, extending from no sensation (rating: -50) to worst pain imaginable (rating: +50), and it explicitly identifies pain threshold (rating: 0).  Here, we tested the SPARS in two experiments using laser heat stimuli to establish its stimulus-response characteristics (Experiment 1, n = 19, 13 stimulus intensities applied 26 times each across a 1-4J range), and to compare it to 0-100 scales that access non-painful (0: no sensation, 100: painful) and painful (0: not painful, 100: worst pain imaginable) events (Experiment 2, n = 7, 9 stimulus intensities applied 36 times each across a 1.5-4.5J range). Despite high inter- and intra-individual variation, we found a reasonably consistent curvilinear stimulus-response relationship (the curve flattens around pain threshold), with stable response characteristics across the range of the scale.  SPARS ratings tended to be lower than the 0-100 pain rating scale in the noxious stimulus intensity range, and greater than the 0-100 non-painful sensation scale in the non-noxious stimulus range; likely reflecting differences in scale dimensionality. The SPARS overcomes limitations of scale range inherent in conventional pain rating scales and, as such, is well suited to experimental studies in which distinguishing between painful and non-painful events is a priority.

## Analysis outputs

**The data required to run the scripts have not been included in the repo. Participants in the studies did not consent to public release of their data, but the data are available from the corresponding author of the published manuscript (see: _Bibliometric information_) on request.**

The outputs from all analysis scripts are located in the _/outputs_ directory. The outputs are formatted as markdown and html. The markdown documents are intermediate outputs generated during the production of the html documents, and while they allow quick browsing of the analysis outputs on GitHub, MathJax formulae and tables are not formatted. 

All inputs (root directory) and outputs (_/outputs_) prefixed with _suppl\_\*\*\__ are analysis scripts cited in the text of the manuscript as supplementary files. For convenience, PDF versions of these files have been included in the _supplements\_pdf_ folder. These PDF files were produced by the scripts contained in the _supplementary\_pdfs_ branch of the SPARS repo (URL: [https://github.com/kamermanpr/SPARS/tree/supplementary_pdfs](https://github.com/kamermanpr/SPARS/tree/supplementary_pdfs)).

## Build the document
While the outputs have been provided, you can build the analysis from scratch on your local machine by following the steps below. The analysis was performed on _R version 3.5.0 (2018-04-23) -- "Joy in Playing"_.

Before building the analysis, _Microsoft Windows_ users must first download and install:

- [Git for Windows](https://github.com/git-for-windows/git/releases) or any other Bash-like shell for _MS Windows_ (download the appropriate 32/64-bit *.exe file).

- [GNU Make](http://gnuwin32.sourceforge.net/downlinks/make.php).

**OPTION 1:**

- Fork the repository to your _GitHub_ account, and then clone the repository to your computer.

- Open a terminal and change the path to the root directory of the repository.

- Type _'make clean'_ (removes all outputs and cleaned data).

- Type _'make'_ (builds the analysis).

**OPTION 2:**

- Download the repo as a zip file, and unzip the file.

- Open a terminal and change the path to the root directory of the unzipped folder.

- Type _'make clean'_ (removes all outputs and cleaned data).

- Type _'make'_ (builds the analysis).
