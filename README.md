# Do Terrorist Attacks Feed Populist Eurosceptics?
---

### Description and data sources

Replication material for 'Do Terrorist Attacks Feed Populist Eurosceptics? Evidence from Two Comparative Quasi-Experiments' published in the European Journal of Political Research. This repository contains all files required to produce the figures, tables and numerical information provided in the manuscript and supplementary material.

### Author/contact

 - Erik Gahner Larsen, University of Kent, E.G.Larsen@kent.ac.uk.
 - David Cutts, University of Birmingham, D.Cutts@bham.ac.uk. 
 - Matthew J. Goodwin, University of Kent, M.J.Goodwin@kent.ac.uk.

### Repository content

- `01_create-data.R` = R script used to create the datasets used for the analysis (requires original data)
- `02_analysis.R` = R script used for all analyses in the article and supplementary material
- `data_ch.csv` = Data from the Chatham House Survey (generated via `01_create-data.R`)
- `data_eb.csv` = Data from the Eurobarometer (generated via `01_create-data.R`)
- `data_ess.csv` = Data from the European Social Survey Survey (generated via `01_create-data.R`)
- `data_gles.csv` = Data from the German Longitudinal Election Study (generated via `01_create-data.R`)
- `sessionInfo.txt` = Output from sessionInfo() in R

### Session info

The analyses were made with [RStudio](http://www.rstudio.com/) (Version 1.1.463) with the following R session:

```
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS Mojave 10.14.4

## Matrix products: default
## BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

## other attached packages:
##  [1] TOSTER_0.3.4      MatchIt_3.0.2     interplot_0.2.1   arm_1.10-1        lme4_1.1-19      
##  [6] Matrix_1.2-15     MASS_7.3-51.1     abind_1.4-5       xtable_1.8-3      stargazer_5.2.2  
## [11] dataMaid_1.2.0    gridExtra_2.3     scales_1.0.0      geofacet_0.1.10   countrycode_1.1.0
## [16] rio_0.5.16        forcats_0.4.0     stringr_1.4.0     dplyr_0.8.0.1     purrr_0.3.2      
## [21] readr_1.3.1       tidyr_0.8.3.9000  tibble_2.1.1      ggplot2_3.1.1     tidyverse_1.2.1  

## loaded via a namespace (and not attached):
##  [1] nlme_3.1-137        svd_0.4.2           sf_0.7-3            lubridate_1.7.4    
##  [5] httr_1.4.0          RItools_0.1-16      tools_3.5.2         backports_1.1.4    
##  [9] R6_2.4.0            rgeos_0.4-2         DBI_1.0.0           lazyeval_0.2.2     
## [13] colorspace_1.4-1    withr_2.1.2         sp_1.3-1            tidyselect_0.2.5   
## [17] curl_3.3            compiler_3.5.2      cli_1.1.0           rvest_0.3.3        
## [21] optmatch_0.9-10     SparseM_1.77        xml2_1.2.0          labeling_0.3       
## [25] DEoptimR_1.0-8      classInt_0.3-1      robustbase_0.93-3   digest_0.6.18      
## [29] foreign_0.8-71      minqa_1.2.4         jpeg_0.1-8          pkgconfig_2.0.2    
## [33] htmltools_0.3.6     rlang_0.3.4.9003    readxl_1.3.1        rstudioapi_0.10    
## [37] generics_0.0.2      jsonlite_1.6        interactionTest_1.1 zip_2.0.1          
## [41] magrittr_1.5        Rcpp_1.0.1          munsell_0.5.0       stringi_1.4.3      
## [45] yaml_2.2.0          geogrid_0.1.1       plyr_1.8.4          ggrepel_0.8.0      
## [49] crayon_1.3.4        lattice_0.20-38     haven_2.1.0         splines_3.5.2      
## [53] pander_0.6.3        hms_0.4.2           zeallot_0.1.0       pillar_1.3.1       
## [57] imguR_1.0.3         glue_1.3.1          data.table_1.12.0   modelr_0.1.4       
## [61] png_0.1-7           vctrs_0.1.0.9003    nloptr_1.2.1        cellranger_1.1.0   
## [65] gtable_0.3.0        assertthat_0.2.1    openxlsx_4.1.0      broom_0.5.2        
## [69] e1071_1.7-1         rnaturalearth_0.1.0 coda_0.19-2         survival_2.43-3    
## [73] class_7.3-14        units_0.6-2        


```
