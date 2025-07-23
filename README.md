# Bullets-and-Ballots
Replication files for UChicago MAPSS Thesis Research

## Maps Folder 
Shape files used for drawing maps for Results Section. 

Source: https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html

## Controls Folder
demographic_data: dataset of polling stations demographic variables (csv)

Creating_demographic_dataset: R code for creating the dataset 

Raw_data: open-source datasets downloaded from TSE, with the information used to construct demographic_data 

# Electoral_data Folder 
geocoded_polling_stations: Daniel Hidalgo's open-source dataset with the coordinates of polling stations in Brazil (csv) 

### RJ 
Raw_data: open-source datasets downloaded from TSE-RJ with voting by candidate by polling station by election year (votacao_secao_...) and candidates' information, including ballot box name (consulta_cand_...)

RJ_POLLING_VOTES_2014: dataset for 2014 polling stations' electoral data, including share of votes for law-and-order candidates (csv)

RJ_POLLING_VOTES_2018: dataset for 2018 polling stations' electoral data, including share of votes for law-and-order candidates (csv)

RJ_POLLING_VOTES_2022: dataset for 2022 polling stations' electoral data, including share of votes for law-and-order candidates (csv)

RJ_panel: dataset for polling stations' electoral data in a panel structure (csv)

Data_cleaning: R code for creating the above 4 datasets

### PE 
Raw_data: open-source datasets downloaded from TSE-PE with voting by candidate by polling station by election year (votacao_secao_...) and candidates' information, including ballot box name (consulta_cand_...)

PE_POLLING_VOTES_2014: dataset for 2014 polling stations' electoral data, including share of votes for law-and-order candidates (csv)

PE_POLLING_VOTES_2018: dataset for 2018 polling stations' electoral data, including share of votes for law-and-order candidates (csv)

PE_POLLING_VOTES_2022: dataset for 2022 polling stations' electoral data, including share of votes for law-and-order candidates (csv)

PE_panel: dataset for polling stations' electoral data in a panel structure (csv)

Data_cleaning: R code for creating the above 4 datasets

## FogoCruzado_Gunshot

gunshots: cleaned dataset with the dates and coordinates of occurrences for Greater Rio de Janeiro and Recife (csv)

Data_cleaning: R code used to create the above dataset from the data extracted from Fogo Cruzado official API

### Raw_data 
FogoCruzado_API_Code: Jupyter Notebook file with Python code used to extract data from Fogo Cruzado's API

ocorrencias_PE: extracted data with gunshot occurrences for Greater Recife (csv)

ocorrencias_RJ:  extracted data with gunshot occurrences for Greater Rio de Janeiro (csv)

## Regressions_BalanceCheck_Maps 
R code for final cleaning, crossing different data, building datasets by polling station with all relevant variables (RJ_ALL_VARIABLES.csv and PE_ALL_VARIABLES.csv), carrying out regressions from sections 5 and 6, doing balance checks, and drawing maps.

## Descriptive Analysis 
R code for drawing figures 1, 2 and 3.

## Discontinuity Graphs 
R code for drawing figure 4

