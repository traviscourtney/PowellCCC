# Group species by genus using common list of species for both NPS and CSUN dataframes
SJnps_calc_sp=SJnps_calc_sp[ , order(colnames(SJnps_calc_sp))]

Orbicella_annularis_complex=data.matrix(rowSums(SJnps_calc_sp[,43:46]))

SJnps_calc_sp = cbind(SJnps_calc_sp[,1:42],Orbicella_annularis_complex,SJnps_calc_sp[,47:ncol(SJnps_calc_sp)])

Unknown_Scleractinia=data.matrix(rowSums(SJnps_calc_sp[,53:54]))
SJnps_calc_sp = cbind(SJnps_calc_sp[,1:52],Unknown_Scleractinia,SJnps_calc_sp[,55:ncol(SJnps_calc_sp)])

Madracis_mirabilis = data.matrix(0*SJnps_calc_sp[,54])
SJnps_calc_sp = cbind(SJnps_calc_sp[,1:24],Madracis_mirabilis,SJnps_calc_sp[,25:ncol(SJnps_calc_sp)])

colnames(SJnps_calc_sp) = 
  c("Acropora_cervicornis",
    "Acropora_palmata",
    "Acropora_prolifera",
    "Agaricia_agaricites",
    "Agaricia_fragilis",
    "Agaricia_grahamae",
    "Agaricia_humilis",
    "Agaricia_lamarcki",
    "Agaricia_spp.",
    "Agaricia_tenuifolia",
    "Agaricia_undata",
    "Agaricia_undata",
    "Colpophyllia_natans",
    "Dendrogyra_cylindrus",
    "Dichocoenia_stokesii",
    "Diploria_labyrinthiformis",
    "Eusmilia_fastigiata",
    "Favia_fragum",
    "Helioseris_cucullata",
    "Isophyllia_rigida",
    "Isophyllia_sinuosa",
    "Madracis_auretenra",
    "Madracis_decactis",
    "Madracis_formosa",
    "Madracis_mirabilis",
    "Madracis_pharensis",
    "Madracis_senaria",
    "Madracis_sp.",
    "Manicina_areolata",
    "Meandrina_jacksoni",
    "Meandrina_meandrites",
    "Millepora_alcicornis",
    "Millepora_complanata",
    "Millepora_sp.",
    "Millepora_squarosa",
    "Montastraea_cavernosa",
    "Mussa_angulosa",
    "Mycetophyllia_aliciae",
    "Mycetophyllia_danaana",
    "Mycetophyllia_ferox",
    "Mycetophyllia_lamarckiana",
    "Mycetophyllia_spp.",
    "Oculina_diffusa",
    "Orbicella_annularis_complex",
    "Porites_astreoides",
    "Porites_branneri",
    "Porites_divaricata",
    "Porites_furcata",
    "Porites_porites",
    "Porites_porites_complex",
    "Pseudodiploria_clivosa",
    "Pseudodiploria_sp.",
    "Pseudodiploria_strigosa",
    "Scleractinia",
    "Scolymia_cubensis",
    "Scolymia_lacera",
    "Scolymia_spp.",
    "Siderastrea_radians",
    "Siderastrea_siderea",
    "Siderastrea_sp.",
    "Solenastrea_bournoni",
    "Solenastrea_hyades",
    "Solenastrea_sp.",
    "Stephanocoenia_intersepta",
    "Tubastrea_aurea")

# Group species by genus using common list of species for both NPS and CSUN dataframes
SJcsun_calc_sp=SJcsun_calc_sp[ , order(colnames(SJcsun_calc_sp))]

Orbicella_annularis_complex2=data.matrix(rowSums(SJcsun_calc_sp[,20:22]))
colnames(Orbicella_annularis_complex2)=c("Orbicella_annularis_complex")

SJcsun_calc_sp = cbind(SJcsun_calc_sp[,1:19],Orbicella_annularis_complex2,SJcsun_calc_sp[,23:ncol(SJcsun_calc_sp)])

SJcsun_missing_taxa_names = 
  c("Acropora_prolifera",
    "Agaricia_fragilis",
    "Agaricia_grahamae",
    "Agaricia_lamarcki",
    "Agaricia_spp.",
    "Agaricia_tenuifolia",
    "Agaricia_undata",
    "Agaricia_undata.1",
    "Helioseris_cucullata",
    "Isophyllia_rigida",
    "Madracis_auretenra",
    "Madracis_formosa",
    "Madracis_pharensis",
    "Madracis_senaria",
    "Madracis_sp.",
    "Meandrina_jacksoni",
    "Millepora_alcicornis",
    "Millepora_complanata",
    "Millepora_sp.",
    "Millepora_squarosa",
    "Mycetophyllia_danaana",
    "Mycetophyllia_ferox",
    "Mycetophyllia_spp.",
    "Oculina_diffusa",
    "Porites_branneri",
    "Porites_porites_complex",
    "Pseudodiploria_clivosa",
    "Pseudodiploria_sp.",
    "Scleractinia",
    "Scolymia_lacera",
    "Scolymia_spp.",
    "Siderastrea_sp.",
    "Solenastrea_bournoni",
    "Solenastrea_hyades",
    "Solenastrea_sp.",
    "Tubastrea_aurea")

#Populate taxa not present in CSUN data with 0 before combining with NPS data
SJcsun_missing_taxa <- as.data.frame(matrix(0, ncol = nrow(as.data.frame(SJcsun_missing_taxa_names)), nrow = nrow(SJcsun_calc_sp)))
colnames(SJcsun_missing_taxa) = SJcsun_missing_taxa_names
SJcsun_calc_sp = cbind(SJcsun_calc_sp,SJcsun_missing_taxa)
SJcsun_calc_sp=SJcsun_calc_sp[ , order(colnames(SJcsun_calc_sp))]
colnames(SJcsun_calc_sp) = colnames(SJnps_calc_sp)

#combine dataframes from both NPS and CSUN dataframes
SJ_calc_sp = rbind(SJnps_calc_sp,SJcsun_calc_sp)
SJ_budget = rbind(SJnps_budget,SJcsun_budget)
SJ_PerCC = rbind(SJnps_PerCC,SJcsun_PerCC)
SJ_CaCO3 = rbind(SJnps_CaCO3,SJcsun_CaCO3)
SJ_taxa = rbind(SJnps_taxa[c(1:4)],SJcsun_taxa[c(1:4)])

# Remove all species with 0 CCC for all sites all years
SJ_calc_sp = SJ_calc_sp[,colSums(SJ_calc_sp^2) !=0]

# Calculate %-total CCC by species for each site for each year
SJ_Percalc_sp = matrix(nrow=nrow(SJ_calc_sp),ncol=ncol(SJ_calc_sp))
for (j in 1:nrow(SJ_calc_sp))
{for (i in 1:ncol(SJ_calc_sp))
{SJ_Percalc_sp[j,i] = (SJ_calc_sp[j,i]/SJ_budget[j,])*100}}
colnames(SJ_Percalc_sp)=colnames(SJ_calc_sp)
SJ_PerCaCO3_Budget = cbind(SJ_taxa[c(1:4)],SJ_PerCC,SJ_CaCO3,SJ_Percalc_sp)

# Calculate mean ± sd percent CCC for each species by year
SJ_per_sp_mean = matrix(nrow=length(1999:2015),ncol=ncol(SJ_Percalc_sp))
for (i in 1999:2015)
{SJ_persub = subset(SJ_PerCaCO3_Budget, SJ_PerCaCO3_Budget$Year == i)
SJ_per_sp_mean[i-1998,] = apply(SJ_persub[7:ncol(SJ_PerCaCO3_Budget)],2,mean)}
colnames(SJ_per_sp_mean)=colnames(SJ_calc_sp)

# Create dataframe with site data, years, total CCC, and CCC per species
SJ_CaCO3_Budget = cbind(SJ_taxa[c(1:4)],SJ_PerCC,SJ_CaCO3,SJ_calc_sp)
SJyear = SJ_CaCO3_Budget$year
SJsite = SJ_CaCO3_Budget$site
# generate annual averages ± confidence intervals of CCC per species across all sites
SJ_sp_mean = matrix(nrow=length(1999:2015),ncol=ncol(SJ_calc_sp))
SJ_CCC_mean = matrix(nrow=length(1999:2015),ncol=1)
SJ_CCC_conf = matrix(nrow=length(1999:2015),ncol=1)
for (i in 1999:2015)
{SJ_persub = subset(SJ_CaCO3_Budget, SJ_CaCO3_Budget$Year == i)
SJ_sp_mean[i-1998,] = apply(SJ_persub[7:ncol(SJ_CaCO3_Budget)],2,mean)
SJ_CCC_mean[i-1998,] = apply(SJ_persub[6],2,mean)
SJ_CCC_conf[i-1998,] = 1.96*(apply(SJ_persub[6],2,sd)/sqrt(nrow(SJ_persub[6])))}
colnames(SJ_sp_mean)=colnames(SJ_calc_sp)

#generate year column
year = 1999:2015

#Create dataframe with year, meanCCC, and CCC confidence intervals
SJ_CCC_unc = data.frame(cbind(year,SJ_CCC_mean,SJ_CCC_conf))
colnames(SJ_CCC_unc)=c("year","meanCCC","CCC_conf")

# Index out all species with < threshold % of total CCC
Threshold = 5
SJ_per_sp_mean_dominant = SJ_per_sp_mean[,!colMeans(SJ_per_sp_mean)<Threshold]
SJ_per_sp_mean_nondominant=data.matrix(rowSums(SJ_per_sp_mean[,-which(colnames(SJ_per_sp_mean) %in% c(colnames(SJ_per_sp_mean_dominant)))]))
colnames(SJ_per_sp_mean_nondominant) = c("Other")
SJ_per_sp_year = data.matrix(cbind(year,SJ_per_sp_mean_dominant,SJ_per_sp_mean_nondominant))

# index dominant and nondominant species using the threshold percent value
SJ_sp_mean_dominant = SJ_sp_mean[,which(colnames(SJ_per_sp_mean) %in% c(colnames(SJ_per_sp_mean_dominant)))]
SJ_sp_mean_nondominant = data.matrix(rowSums(SJ_sp_mean[,-which(colnames(SJ_per_sp_mean) %in% c(colnames(SJ_per_sp_mean_dominant)))]))
colnames(SJ_sp_mean_nondominant) = c("Other")
SJ_sp_year = data.matrix(cbind(year,SJ_sp_mean_dominant,SJ_sp_mean_nondominant))

#Melt the data to specieste stacked bar plot in ggplot
SJ_melt=melt(data.frame(SJ_sp_year), id = c("year"))

#automate lme of species CCC vs. year with random slopes and intercepts by site
formulas = paste(noquote(colnames(SJ_per_sp_mean_dominant)), noquote(replicate(length(colnames(SJ_per_sp_mean_dominant)), "~ Year")), sep = " ")
ctrl <- lmeControl(opt='optim');
SJlmefits = lapply(formulas, function(mm) summary(lme(as.formula(mm), random = ~Year|Site_name, corr = corCAR1(form = ~Year|Site_name), data=SJ_PerCaCO3_Budget,control=ctrl,method = "ML")))
SJslopes = data.frame(lapply(SJlmefits, function(x) coef(x)[2,1]))
colnames(SJslopes) = colnames(SJ_per_sp_mean_dominant)
SJslopes_err = data.frame(lapply(SJlmefits, function(x) coef(x)[2,2]))
colnames(SJslopes_err) = colnames(SJslopes)
SJslopes_df = data.frame(lapply(SJlmefits, function(x) coef(x)[2,3]))
colnames(SJslopes_df) = colnames(SJslopes)
SJslopes_t = data.frame(lapply(SJlmefits, function(x) coef(x)[2,4]))
colnames(SJslopes_t) = colnames(SJslopes)
SJslopes_p = data.frame(lapply(SJlmefits, function(x) coef(x)[2,5]))
colnames(SJslopes_p) = colnames(SJslopes)

#generate summary of slopes ± standard error and p-values for all dominant coral species 
SJ_lme_summary = rbind(colnames(SJ_per_sp_mean_dominant),as.numeric(SJslopes),SJslopes_err,SJslopes_df,SJslopes_t,SJslopes_p)
SJ_lme_summary = as.data.frame(t(SJ_lme_summary))
colnames(SJ_lme_summary) = c("species","slope","std_error","df","t","pvalue")
SJ_lme_summary$slope=as.numeric(as.character(SJ_lme_summary$slope))
SJ_lme_summary$std_error=as.numeric(as.character(SJ_lme_summary$std_error))
SJ_lme_summary$df=as.numeric(as.character(SJ_lme_summary$df))
SJ_lme_summary$t=as.numeric(as.character(SJ_lme_summary$t))
SJ_lme_summary$pvalue=as.numeric(as.character(SJ_lme_summary$pvalue))
SJ_lme_summary

# generate coral cover ranging from 0 to 100% for simulated potential CCC by genera
cover = seq(0,100,by=0.01)

# Generate potential CCC by species for the dominant calcifying corals from St. John
SJ_Montastrea_cavernosa = cover/100*10.8
SJ_Orbicella_annularis_complex = cover/100*10.5
SJ_Porites_astreoides = cover/100*6.3
SJ_Porites_porites = cover/100*8.5
SJ_Siderastrea_siderea = cover/100*7.4

SJ_potential = data.frame(cbind(cover,SJ_Montastrea_cavernosa,SJ_Orbicella_annularis_complex,SJ_Porites_astreoides,SJ_Porites_porites,SJ_Siderastrea_siderea))
