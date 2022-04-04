# deepPseudoMSI

Deep learning-based pseudo mass spectrometry imaging processing for precision medicine.

More information can be found [here](https://www.deeppseudomsi.org/).

# Install packages

Please install R packages that used in the analysis.

```
install.packages("remotes")
install.packages("tidyverse")
remotes::install_gitlab("jaspershen/masstools")
install.packages("tidyr")
install.packages("plyr")
install.packages("VennDiagram")
install.packages("randomForest")
install.packages("Boruta")
```

# Code

There are three parts of codes in this repo. 

1. `pseudoMS-image-converter`

This is written by R which is used to convert LC-MS raw data to pseudo-MS image.

2. `pseudoMS-image-predictor`

This is written by Python which is used to construct deep learning prediction model.

3. `figures_in_manuscript`

All the analysis code in our manuscript.
