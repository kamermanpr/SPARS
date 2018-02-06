# FEST 

## Study A: FEST 2015

Background & objectives
HIV-associated sensory neuropathy (HIV-SN) is a common, frequently painful complication of HIV. We investigated whether psychological factors associated with painful versus non-painful HIV-SN, and if pain and psychological factors affected quality of life (QoL). And, in the painful HIV-SN group only (secondary analysis), we investigated whether psychological factors associated with pain intensity.

Methods
We recruited 125 patients with painful HIV-SN and 72 patients with non-painful HIV-SN. We assessed anxiety and depression using the Hopkins Symptoms Checklist-25. Pain catastrophizing and QoL was assessed using the Pain Catastrophizing Scale and EQ-5D, respectively.

Results
No included psychological factors were associated with having painful HIV-SN. Greater depressive symptoms and presence, but not intensity, of pain was independently associated with lower QoL. In addition, a greater depressive symptom score was associated with increased pain intensity.

Conclusion
Our findings demonstrate a high pain burden in this cohort, of which HIV-SN was only one part, and emphasises the importance of psychological factors, particularly depression, in the assessment of HIV-SN. Our data also confirm that results from studies in developed countries cannot be generalised to African cohorts.

# Analysis outputs
The outputs from all analysis scripts are located in the outputs directory. The outputs are formatted as markdown and html. The markdown documents are intemediate outputs generated during the production of the html documents, and while they allow quick browsing of the analysis outputs on GitHub, MathJax formulae and tables are not formatted. 

# Build the document
While the outputs have been provided, you can build the analysis from scratch on your local machine by following the steps below. The analysis was performed on _R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"_, and to aid reproducibility, we have used `packrat` to maintain a local snapshot of package dependencies used in this project. 

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
