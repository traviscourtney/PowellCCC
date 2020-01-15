######################################################################
##’ @title Scripts to analyze coral calcification capacity from Guest et al. (2018)
##’
##’ @author Travis A Courtney
##’ @contact traviscourtney@gmail.com
##’ @date 2020-01–13
##’ @reference Guest et al Generation of Coral Calcification Capacity from Taxa and Rate data
##’ @log Add a log here
######################################################################

#load libraries
library(ggplot2)
library(nlme)
library(reshape2)
library(matrixStats)
library(gridExtra)
library(readr)
library(nlme)

# run required Powell Calcification script from Guest et al. (2018)
source("Powell_Calcification.R")

#run data analysis and plot-making scripts for present study
source("HI_calc_by_species.R")
source("MO_calc_by_genera.R")
source("SJ_calc_by_species.R")
source("FK_calc_by_species.R")
source("CCC_vs_CC_slopes.R")

#create figures (final edits have been made to figures prior to publication)
source("figures.R")

#Print Figure 1 CCC_by_taxa_figure to pdf
pdf("Figure1.pdf", width = 19, height = 10)
grid.arrange(HI_perCalcplot,FK_perCalcplot,MO_perCalcplot,SJ_perCalcplot,ncol=2)
dev.off()

#Print Figure 2 CCC_vs_coral_cover_plot to pdf
pdf("Figure2.pdf", width = 14, height = 10)
grid.arrange(HIpotentialplot,FKpotentialplot,MOpotentialplot,SJpotentialplot,ncol=2)
dev.off()

#Print Figure 3 mean slope of CCC vs coral cover plot to pdf
pdf("Figure3.pdf", width = 10, height = 7)
CCslopeplot
dev.off()

#Print Figure 4 Percent_CCC_change_year_plot to pdf
pdf("Figure4.pdf", width = 14, height = 10)
grid.arrange(HI_species_year_plot,FK_species_year_plot,MO_genus_year_plot,SJ_species_year_plot,ncol=2)
dev.off()

