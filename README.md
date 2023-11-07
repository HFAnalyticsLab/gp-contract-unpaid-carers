# GP Core Contract Data - Unpaid Carers Analysis
Analysis using NHS Digital's GP Core Contract data on unpaid carers registered to GP practices, compared to unpaid carers per Local Authority from the 2021 UK Census.

## Sources ## 
- [GP Contract Services - England, 2023-24 (NHS Digital)](https://digital.nhs.uk/data-and-information/publications/statistical/gp-contract-services/2023-24)
- [Patients registered at a GP practice, October 2023 (NHS Digital)](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/october-2023)
- [Census 2021: Provision of Unpaid Care (Office for National Statistics)](https://www.ons.gov.uk/datasets/TS039/editions/2021/versions/3)
- [LSOA (2011) to LSOA (2021) to Local Authority District (2022) Lookup for England and Wales (ONS Open Geography Portal)](https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2011-to-lsoa-2021-to-local-authority-district-2022-lookup-for-england-and-wales-version-2/about) 

## Background ## 

NHS Digital’s “GP Contract Services – England” dataset provides information on primary care programmes including core contract components, enhanced services, vaccination and immunisation, and indicators formerly included in the Quality and Outcomes Framework (QOF). A new indicator on numbers of unpaid carers registered to each GP practice was added to the GP contract data for 2022-23 Q1, and has appeared in each new edition of the dataset since then. This dataset compares the number of unpaid carers according to the GP core contract data to the number of unpaid carers recorded in the 2021 census on a local authority level.  

## Approach and data preparation ## 

To make a comparison between unpaid carers registered by GP practice (from the GP Contract Services data, 2023-24 Q2 vintage) and the number of unpaid carers by local authority reported in the 2021 census, the practice-level GP contract data must be transformed into local authority-level data. 

This transformation was performed by linking the GP contract data to NHS Digital’s “Patients Registered at a GP Practice – LSOA” data (July 2023 vintage), which details the number of patients registered to each GP practice by Lower Super Output Area (LSOA) of residence. The numbers of unpaid carers registered to each practice are then proportionally assigned to LSOAs by the percentage of the given practice’s patients residing in each LSOA. These estimates for GP-registered unpaid carers in each LSOA are then summed to a Local Authority level for comparison to the census. 

Mapping the GP contract data to an LSOA level is complicated by the fact that the “Patients Registered at a GP Practice” data uses an outdated and incomplete set of LSOA codes, drawn from the 2011 census. Registered patients residing in LSOAs which did not exist in 2011 are given the code ‘NO2011’ in the raw data, meaning that while we know which practice these patients are assigned to, we do not know which LSOA or range of LSOAs they reside in, in either 2021 or 2011 terms. This affects relatively few registered patients, with only 0.037% being assigned this LSOA category. To remedy this issue, patients given a ‘NO2011’ LSOA code for each practice were assigned to the LSOA where the plurality of the practice’s patients resided. 


## Scripts ## 

**00_project_setup.R**
This script loads in necessary packages and sets up Raw_data and Outputs folders in the working directory. It then downloads the necessary raw data for the analysis directly from their online sources, which are stored in the Raw_data folder. Two sources (ONS' 2021 census data and the LSOA to LA lookup reference file from the ONS Open Geography Portal) are saved as RDS files in the _Resources_ folder, as there was no straightforward method of directly downloading them from their online sources. 

**01_gp-practice-LA-mapping.R**
This script loads in, processes and transforms the GP Core Contract data on unpaid carers registered by GP practice to a local authority level, following the methods outlined above in _Approach and data preparation_. It then links this transformed data to the counts of unpaid carers per local authority from the 2021 census, and compares the two measures.

## Outputs ## 
These scripts return the following outputs:
- _GP_unpaid_carers_by_LSOA.csv_, a csv file featuring the full breakdown of unpaid carers by GP practice profiled to an LSOa level
- _GP_contract_to_census_LA_comparison.csv_, a csv file featuring the full local authority level comparison between the transformed and aggregated GP core contract data and the 2021 census data on unpaid carers
