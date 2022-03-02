# Pediatric_LT_Mdx

The Rscripts folder contains all the codes we used for the analysis

The data is contained in the dataset folder



Mdx liver transplant data
1. calculateModuleStat_LogBased_Variation2.R
-- This script calculates the module specific log-based score for each of the 555 modules
2. CV_LassoSVM_NetFeatures_LogBased_V0.R
-- Runs a cross validated (lasso + SVM) model on the module-specific score (score is caculated based on the gene of each module)
3. Plot_Box_Lasso_RF_SVM_All_Final.R
-- This script plots all the original vs permuted AUC plots
4. Plot_PLS_LassoSelected_Features.R
-- This script plots the partial least square regression on the lasso selected features/modules for pre, post-early, and post-late module specific data
5. Plot_LassoSelected_Features_Variations.R
-- This script plots the heatmap of lasso selected features/modules for pre, post-early, and post-late module specific data
6. find_FC_OfSelectedGenes.R
-- This script calculates the fold change (fc) scores of the genes of the Modules. We use these fc scores to visualize the modules and the genes.

