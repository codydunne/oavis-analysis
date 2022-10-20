# OAVIS Analysis

An analysis of the 2017–2019 data provided by [the OAVIS project](https://github.com/steveharoz/open-access-vis) using their R code, Excel, and Tableau.

## Issues with data completeness

The data collected in OAVIS is incomplete, in part due to the difficulty of collecting all this data manually and in part due to link rot since the data was collected 2017–2019. See the contribution instructions at https://github.com/steveharoz/open-access-vis to make corrections.

## Analysis process

1. Combine all the OAVIS data files in `oavis-data` using Excel's Data→Get Data→Get Data From Folder functionality, as the columns vary. Save as `openaccessvis-all.xlsx` and `openaccessvis-all.csv`
2. Run `oavis analysis.R` to generate `openaccessvis-all-processed.csv` and a bunch of images.
3. Load in Tableau and edit to create `openaccessvis-all-processed.twb` and export `openaccessvis-all-processed.pdf`
