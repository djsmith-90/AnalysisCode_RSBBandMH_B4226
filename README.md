## Git repository for 'Bidirectional causality between religion and mental health' ALSPAC project (B4226)

The main directory in this repository contains six R scripts for the actual ALSPAC analyses. These files are:
 - Script1a_DataProcessingAndCleaning.R - Script to clean and process the raw ALSPAC data
 - Script1b_CreatingSyntheticDatasets.R - Script to create synthetic datasets using the 'synthpop' package
 - Script2a_MothersAnalyses_RSBBCauseMH.R - Script to analyse the mothers data with RSBB as exposure and MH as outcome
 - Script2b_MothersAnalyses_MHCauseRSBB.R - Script to analyse the mothers data with MH as exposure and RSBB as outcome
 - Script3a_PartnersAnalyses_RSBBCauseMH.R - Script to analyse the partners data with RSBB as exposure and MH as outcome
 - Script3b_PartnersAnalyses_MHCauseRSBB.R - Script to analyse the partners data with MH as exposure and RSBB as outcome

 
The 'SyntheticData' folder also contains synthetic versions of the mother's and partner's ALSPAC datasets, created
using Script 1b above. As raw ALSPAC data cannot be released, these synthesised datasets are modelled on the original 
ALSPAC data, thus maintaining variable distributions and relations among variables (albeit not pefectly), while 
at the same time preserving participant anonymity and confidentiality. Please note that while these synthetic datasets 
can be used to follow the analysis scripts, as data are simulated they should *not* be used for research purposes; 
only the actual, observed, ALSPAC data should be used for formal research and analyses reported in published work.

These mother's synthetic datasets have the file names 'syntheticData_mum_mhOutcome_B4226' (where RSBB is the exposure 
and MH the outcome) and 'syntheticData_mum_rsbbOutcome_B4226' (where MH is the exposure and MH the outcome), while 
for partners the file names are 'syntheticData_partner_mhOutcome_B4226' (where RSBB is the exposure and MH the outcome) 
and 'syntheticData_partner_rsbbOutcome_B4226' (where MH is the exposure and MH the outcome). All files are available in 
both R ('.RData') and CSV ('.csv') formats.


Note that ALSPAC data access is through a system of managed open access. Information about access to ALSPAC data is 
given on the ALSPAC website (http://www.bristol.ac.uk/alspac/researchers/access/). The datasets used in these
scripts are linked to ALSPAC project number B4226; if you are interested in accessing these datasets, please quote 
this number during your application.
