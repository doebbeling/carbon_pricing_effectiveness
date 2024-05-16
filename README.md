# carbon_pricing_effectiveness
Systematic review and meta-analysis of ex-post evaluations on the effectiveness of carbon pricing

The findings of this project are published in:
Döbbeling-Hildebrandt, N., Miersch, K., Khanna, T.M., Bachelet, M., Bruns, S., Callaghan, M., Edenhofer, O., Flachsland, C., Forster, P., Kalkuhl, M., Koch, N., Lamb, W., Ohlendorf, N., Steckel, J.C., Minx, J.C. (2024) Systematic review and meta-analysis of ex-post evaluations on the effectiveness of carbon pricing. Nat Commun 15, 4147. https://doi.org/10.1038/s41467-024-48512-w.

More information on the method and findings can be found in the linked research article. 

We provide meta-data for all screened documents. Screened documents form the title and abstract screening together with their inclusion decision and screening order are provided in screening_data.csv. For full text screening the documents are split into excluded documents (CMexc) and included documents. Included documents are provided together with the extracted study and effect size information as specified below.  

The study and effect size information is provided before any transformations were conducted (CMinc_raw) as well as after harmonisation (CMinc_final). Both datasets are provided as CSV as well as RData formats.
The R skripts were used for data harmonisation (effect_size_harmonisation) and for the mata-analysis (meta_analysis).

The provided code can be run in any R application (i.e. RStudio). It was written and executed using the below specified versions of R and the packages.
The code should not require any specific hardware, operating systems or software dependencies other than the R packages specified below.
The code is commented and provides step-by-step explanations of the data harmonisation and analysis. If questions arise do not hesitate to contact us.

R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] BMS_0.3.5       metafor_3.8-1   metadat_1.2-0   Matrix_1.5-3    forcats_0.5.2   stringr_1.4.1   dplyr_1.0.10    purrr_0.3.5     readr_2.1.3     tidyr_1.2.1     tibble_3.1.8   
[12] ggplot2_3.4.0   tidyverse_1.3.2

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.0    xfun_0.34           lattice_0.20-45     haven_2.5.1         gargle_1.2.1        colorspace_2.0-3    vctrs_0.5.0         generics_0.1.3      htmltools_0.5.3    
[10] yaml_2.3.6          utf8_1.2.2          rlang_1.0.6         pillar_1.8.1        glue_1.6.2          withr_2.5.0         DBI_1.1.3           dbplyr_2.2.1        modelr_0.1.10      
[19] readxl_1.4.1        lifecycle_1.0.3     munsell_0.5.0       gtable_0.3.1        cellranger_1.1.0    rvest_1.0.3         evaluate_0.18       knitr_1.41          tzdb_0.3.0         
[28] fastmap_1.1.0       fansi_1.0.3         broom_1.0.1         scales_1.2.1        backports_1.4.1     googlesheets4_1.0.1 jsonlite_1.8.3      fs_1.5.2            hms_1.1.2          
[37] digest_0.6.30       stringi_1.7.8       mathjaxr_1.6-0      grid_4.2.2          cli_3.4.1           tools_4.2.2         magrittr_2.0.3      crayon_1.5.2        pkgconfig_2.0.3    
[46] ellipsis_0.3.2      xml2_1.3.3          reprex_2.0.2        googledrive_2.0.0   lubridate_1.9.0     timechange_0.1.1    assertthat_0.2.1    rmarkdown_2.18      httr_1.4.4         
[55] rstudioapi_0.14     R6_2.5.1            nlme_3.1-160        compiler_4.2.2 
