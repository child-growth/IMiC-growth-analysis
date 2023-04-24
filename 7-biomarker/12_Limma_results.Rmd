---
title: |
  Limma Results
author: "Sajia Darwish"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
output:
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
library(knitr)
library(gplots)
opts_chunk$set(echo = FALSE,
               out.width = "75%", 
               fig.align = "center",
               fig.show = 'hold')
```

### BiocNorm: Elicit and Vital
```{r,echo=FALSE,out.width='.49\\linewidth',fig.width=3,fig.height=3,fig.show='hold',fig.align='center'}
include_graphics(c("/data/imic/results/figures/limma/volcano_elicit_biocNorm.png",
                   "/data/imic/results/figures/limma/volcano_vital_biocNorm.png"))
```

### MetabInd: Elicit and Vital
```{r,echo=FALSE,out.width='.49\\linewidth',fig.width=3,fig.height=3,fig.show='hold',fig.align='center'}
include_graphics(c("/data/imic/results/figures/limma/volcano_elicit_metabInd.png",
                   "/data/imic/results/figures/limma/volcano_vital_metabolInd.png"))
```

### HMO: Elicit and Vital
```{r,echo=FALSE,out.width='.49\\linewidth',fig.width=3,fig.height=3,fig.show='hold',fig.align='center'}
include_graphics(c("/data/imic/results/figures/limma/volcano_elicit_hmo.png",
                   "/data/imic/results/figures/limma/volcano_vital_hmo.png"))
```

### Sapient: Elicit and Vital
```{r,echo=FALSE,out.width='.49\\linewidth',fig.width=3,fig.height=3,fig.show='hold',fig.align='center'}
include_graphics(c("/data/imic/results/figures/limma/volcano_elicit_sapient.png",
                   "/data/imic/results/figures/limma/volcano_vital_sapient.png"))
```

### Bvitamin: Elicit
```{r,echo=FALSE,out.width='.49\\linewidth',fig.width=3,fig.height=3,fig.show='hold',fig.align='center'}
include_graphics("/data/imic/results/figures/limma/volcano_elicit_bvit.png")
```

### Bpl: Vital
```{r,echo=FALSE,out.width='.49\\linewidth',fig.width=3,fig.height=3,fig.show='hold',fig.align='center'}
include_graphics("/data/imic/results/figures/limma/volcano_vital_pbl.png")
```

\newpage

## Differential expression of biomarkers shared between sites

1) Average expression after 5% FDR cutoff
2) Biomarkers shared between sites: non-common biomarkers not included.
3) Helps to see consistency of shared biomarkers across sites.

### 1) HMO: sugars
```{r, echo = FALSE}

include_graphics("/data/imic/results/figures/limma/hmoHeat.png")
```

### 2) BiocNorm: metabolites
```{r, echo = FALSE}

include_graphics("/data/imic/results/figures/limma/biocNormHeat.png")
```

### 3) Metabolomic indicators: sums and ratios
```{r, echo = FALSE}
include_graphics("/data/imic/results/figures/limma/metabHeat.png")
```

### 4) Sapient: untargetd metabolomics
```{r, echo = FALSE}
include_graphics("/data/imic/results/figures/limma/sapientHeat.png")
```

## Next step:

1) Look at the expression of non-common biomarkers across sites.
2) Incorporate classification of calculated outcomes (e.g., stunting).

















