# Tidy GHG Inventories

This repository contains code to build a local database of national GHG inventories, based on the following components:

### compile_crt_files 
- identify new CRTs on the UNFCCC website, save these locally, extract metadata
### compile_crt_summaries
- extract GHG emissions data from the latest CRT and save into a structured, consistent format
### compile_crt_energy
- extract detailed GHG emissions data in the energy sector from individual CRT files, check and try to ensure consistency with the summary sheets and save into a structured, consistent format
### compile_tidy_inventories
- combine all the latest locally saved data into a single cross-national inventory sheet

