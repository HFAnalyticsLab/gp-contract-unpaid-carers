# gp-contract-unpaid-carers
Analysis using NHS Digital's GP Core Contract data on unpaid carers registered to GP practices, compared to unpaid carers per Local Authority from the 2021 UK Census.

## Sources ## 

## Approach and data preparation ## 

To make a comparison between unpaid carers registered by GP practice (from the GP Contract Services data, 2023-24 Q1 vintage) and the number of unpaid carers by local authority reported in the 2021 census, the practice-level GP contract data must be transformed into local authority-level data. 
This transformation was performed by linking the GP contract data to NHS Digital’s “Patients Registered at a GP Practice – LSOA” data (July 2023 vintage), which details the number of patients registered to each GP practice by Lower Super Output Area (LSOA) of residence. The numbers of unpaid carers registered to each practice are then proportionally assigned to LSOAs by the percentage of the given practice’s patients residing in each LSOA. These estimates for GP-registered unpaid carers in each LSOA are then summed to a Local Authority level for comparison to the census. 

Mapping the GP contract data to an LSOA level is complicated by the fact that the “Patients Registered at a GP Practice” data uses an outdated and incomplete set of LSOA codes, drawn from the 2011 census. Registered patients residing in LSOAs which did not exist in 2011 are given the code ‘NO2011’ in the raw data, meaning that while we know which practice these patients are assigned to, we do not know which LSOA or range of LSOAs they reside in, in either 2021 or 2011 terms. This affects relatively few registered patients, with only 23,317 out of a total 62.6 million registered in the data (0.037%) being assigned this LSOA category. To remedy this issue, patients given a ‘NO2011’ LSOA code for each practice were assigned to the LSOA where the plurality of the practice’s patients resided. 

One practice in the GP contract data is missing from the “Patients Registered at a GP Practice” data, namely Rosegarth Surgery, West Yorkshire. This practice has no unpaid carers registered, so does not affect our analysis. All other practices featured in the GP contract data were successfully matched in the registered patients data.
Conversely, 76 practices featured in the “Patients Registered at a GP Practice” dataset are missing from the GP contract data, therefore lacking any data on unpaid carers registered to that practice. Together, these missing practices account for 388,061 patients. In order to identify which LAs are missing practices within their boundaries in the GP contract data, data on the postcodes of active GP practices were drawn from NHS Digital, which in turn were linked to LSOA and LA-level aggregations using lookups from the ONS Geography Portal. 

## Scripts ## 
