# ENV_872_Project

## Summary
This repository was prepared to hold data, code, and other output for Duke University's Environmental Data Analytics (ENV 872) Spring 2020 final project. The analysis for this project will focus on the effects of different marine zones within Australia's Great Barrier Reef on fish density and live coral cover as well as shifts in these fish and benthic populations over time.

## Investigators
This repository was created and assembled by Claire Mullaney, a Duke University Master of Environmental Management student (claire.mullaney@duke.edu). 

## Keywords
Great Barrier Reef, coral reefs, fish, benthos, complexity, coral cover, marine zoning, marine spatial planning, marine science

## Database Information
Data were collected by by the ARC Centre of Excellence for Coral Reef Studies at James Cook University. They were accessed via the Australian government's database for open government data, data.gov.au: https://data.gov.au/data/dataset/031f0668-b874-48fc-a058-f146c2f6fc69

Data files were hosted in the eAtlas data repository: http://eatlas.org.au/pydio/data/public/833ece.php 

More information on data.gov.au can be found here: https://search.data.gov.au/page/about

More information on eAtlas can be found here: https://eatlas.org.au/content/about-e-atlas

From the downloaded dataset, `E-Atlas Data_NERP 8.2_Dec 2014_fish_benthos.xlsx` was placed in the repository for analysis inside the Data/Raw folder. This file had two sheets; the first was saved as `data.gov.au_fish_benthos_GBR_sites_raw.csv` and the second was saved as `data.gov.au_fish_benthos_GBR_zones_raw.csv`.

`E-Atlas Data_NERP 8.2_Site Coordinates.xlsx` was saved in the Data/Metadata folder as `data.gov.au_site_coordinates.csv`.

`Fish Species List_GBR Monitoring_Functional groups & fishery status_2014.xlsx` was saved in the Data/Metadata folder as `data.gov.au_fish_groups_status.csv`.

`e-Atlas-dataset-report-form_NERP 8.2 Dec 2014_fish_benthos.docx` was saved in the Data/Metadata folder as `data.gov.au_fish_benthos_dataset_info.pdf`.

Data were accessed 2020-04-10.

## Folder structure, file formats, and naming conventions 

Folders contained in the repository include:

Folder/Pathway | Description | File Format(s)
---------------| ----------- | -----------
**Code** | R scripts created to explore, wrangle, and analyze data | .r
**Data/Raw** | Data files in their original formatting | .csv
**Data/Processed** | Revised data files created by wrangling raw data files | .csv
**Data/Metadata** | Files giving additional information about raw data | .csv, .pdf
**Output** | R Markdown files and PDFs that will be used as final products | .rmd, .pdf

***

Files in the data folder are named according to the following convention: `database_description_details_stage.format`.

**database** is the database from which the data were obtained

**description** is a description of the data 

**details** are additional details necessary to understand the content of the dataset

**stage** indicates the point where the file is located in the data science pipeline (in these files, stage will either be "raw" or "processed")

**format** is the file's format (e.g., .csv)

## Metadata

These data, which were collected between 1999 and 2014 on the inshore coral reefs of the Great Barrier Reef Marine Park, contain the average percent cover of major benthic categories as well as the average density of fish functional groups. `data.gov.au_fish_benthos_GBR_sites_raw.csv` gives these data averaged for each site while `data.gov.au_fish_benthos_GBR_zones_raw.csv` gives them averaged for each zone (see table below for a description of the site and zone variables).

Underwater visual census (UVC) methodology was used to survey these fish and benthic communities. Within each site, UVC surveys were conducted using 5 replicate transects (each 50m x 6m) deployed on the reef slope between approximately 4 and 12 meters. To enable the assessment of the effects of a rezoning plan that was implemented in 2004, there are even numbers of sites located in zones where fishing is allowed, zones that were closed to fishing in 1987, and zones that were closed to fishing in 2004.

Both `data.gov.au_fish_benthos_GBR_sites_raw.csv` and `data.gov.au_fish_benthos_GBR_zones_raw.csv` contain the following columns: 

Dataset Column | Description | Class
---------------| ----------- | -----
**Year** | Year of data collection | Integer
**Region** | The island group (Palm, Magnetic, Whitsunday or Keppel) of the coral reef that was surveyed | Factor
**Zone** | Indicates if data were collected in a no-take zone established in 1987 (NTR 1987), a no-take zone established in 2004 (NTR 2004), or a zone where fishing is allowed (Fished) | Factor
**Total Fish Densit_mean** | Mean number of fish observed per 1000 m^2^ at the specified site | Numeric
**Total Fish Densit_SE** | The standard error of the mean number of fish observed per 1000 m^2^ at the specified site | Numeric
**Fish Species richness_mean** | The mean number of fish species observed at the specified site | Numeric
**Fish Species richness_SE** | The standard error of the mean number of fish species observed at the specified site | Numeric
**Fishery Target Spp_mean** |  Mean number of fish species designated as 'Primary target’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Fishery Target Spp_SE** | Standard error of the mean number of fish species designated as 'Primary target’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Grazers_mean** | Mean number of fish species listed as ‘grazers’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Grazers_SE** | Standard error of the mean number of fish species listed as ‘grazers’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Corallivores_mean** | Mean number of fish species listed as ‘corallivores’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Corallivores_SE** | Numeric
**Planktivores_mean** | Mean number of fish species listed as ‘planktivores’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Planktivores_SE** | Standard error of the mean number of fish species listed as ‘planktivores’ in the `data.gov.au_fish_groups_status.csv` metadata file  | Numeric
**Territorial Pomacentrids_mean** | Mean number of fish species listed as ‘territorial pomacentrids’ in the `data.gov.au_fish_groups_status.csv` metadata file | Numeric
**Territorial Pomacentrids_SE** | Numeric
**Plectropomus spp_mean** | Mean number of observed fish species in the genus ***Plectropomus*** (i.e., coral groupers; all species of coral groupers observed are listed in the `data.gov.au_fish_groups_status.csv` metadata file) | Numeric
**Plectropomus spp_SE** | Standard error of the mean number of observed fish species in the genus ***Plectropomus*** | Numeric
**SCI_mean** | Mean structural complexity index at each site. Individual SCI values range from 1 to 25, with values closer to 1 indicating less structural complexity and values closer to 25 indicating more structural complexity. Each SCI estimate was calculated by multiplying visual estimates of reef slope angle (1-5) by reef slope rugosity (1-5). These values were estimated for each 10m section of each 50m transect for a total of 5 estimates per transect. With 5 transects deployed per site, a total of 25 SCI values were estimated per site. These 25 values were averaged to obtain the mean SCI. | Numeric
**SCI_SE** | Standard error of the mean SCI at each site | Numeric
**LCC_mean** | Mean live hard and soft coral % cover | Numeric
**LCC_SE** | Standard error of the mean live hard and soft coral % cover | Numeric
**LHC_mean** | Mean live hard coral % cover | Numeric
**LHC_SE** | Standard error of the mean live hard coral % cover | Numeric
**MAC_mean** | Mean macroalgae % cover (includes only fleshy algae, not turf algae) | Numeric
**MAC_SE** | Standard error of the mean macroalgae % cover | Numeric

`data.gov.au_fish_benthos_GBR_sites_raw.csv` contains the following additional column:

Dataset Column | Description | Class
---------------| ----------- | -----
**Site** | The ID of the site (where five transect surveys were conducted) within the specified zone, region, and year | Factor

`data.gov.au_fish_benthos_GBR_zones_raw.csv` contains the following additional column:

Dataset Column | Description | Class
---------------| ----------- | -----
**No. Transect** | The number of transects deployed in the specified zone, region, and year | Integer


Information on dataset and collection methods obtained from: https://data.gov.au/data/dataset/benthic-cover-and-fish-density-on-fringing-reefs-of-inshore-island-groups-of-the-gbr-1999-2014-1

This information is also available within the `data.gov.au_fish_benthos_dataset_info.pdf` file in the Data/Metadata folder.

## Scripts and code

Scripts used to explore, wrangle, and analyze data will be saved as .r files and placed in the **Code** folder.

Finalized and polished code will be saved as .rmd files in the **Output** folder.

## Quality assurance/quality control

All analysis steps and changes made to datasets, along with any significant errors, outliers, and problems that are encountered, will be documented in scripts (.r files) in the Code folder. 

All processed datasets will be named according to the naming conventions outlined above and saved in the Data/Processed folder. 

Modifications to repository materials will be pushed to GitHub regularly to ensure a current backup is maintained.
