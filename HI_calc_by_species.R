#Data from Powell_calcification script to use in the analysis of species contributions to CCC in the Main Hawaiian Islands
HI_calc_sp
HI_taxa = Mhi_CRAMP_taxa

# Remove all species with 0 CCC for all sites all years
HI_calc_sp = HI_calc_sp[,colSums(HI_calc_sp^2) !=0]

# Calculate %-total CCC by species for each site for each year
HI_Percalc_sp = matrix(nrow=nrow(HI_calc_sp),ncol=ncol(HI_calc_sp))
for (j in 1:nrow(HI_calc_sp))
{for (i in 1:ncol(HI_calc_sp))
{HI_Percalc_sp[j,i] = (HI_calc_sp[j,i]/HI_budget[j,])*100}}
colnames(HI_Percalc_sp)=colnames(HI_calc_sp)
HI_PerCaCO3_Budget = cbind(HI_taxa[c(1:4)],HI_PerCC,HI_CaCO3,HI_Percalc_sp)

# Calculate mean CCC for each species by year
HI_per_sp_mean = matrix(nrow=length(1999:2014),ncol=ncol(HI_Percalc_sp))
for (i in 1999:2014)
{HI_persub = subset(HI_PerCaCO3_Budget, HI_PerCaCO3_Budget$year == i)
HI_per_sp_mean[i-1998,] = apply(HI_persub[7:ncol(HI_PerCaCO3_Budget)],2,mean)}
colnames(HI_per_sp_mean)=colnames(HI_calc_sp)

# Create dataframe with site data, years, total CCC, and CCC per species
HI_CaCO3_Budget = cbind(HI_taxa[c(1:4)],HI_PerCC,HI_CaCO3,HI_calc_sp)
HIyear = HI_CaCO3_Budget$year
HIsite = HI_CaCO3_Budget$site

# generate annual averages of CCC ± confidence intervals per species across all sites
HI_sp_mean = matrix(nrow=length(1999:2014),ncol=ncol(HI_calc_sp))
HI_CCC_mean = matrix(nrow=length(1999:2014),ncol=1)
HI_CCC_conf = matrix(nrow=length(1999:2014),ncol=1)
for (i in 1999:2014)
{HI_persub = subset(HI_CaCO3_Budget, HI_CaCO3_Budget$year == i)
HI_sp_mean[i-1998,] = apply(HI_persub[7:ncol(HI_CaCO3_Budget)],2,mean)
HI_CCC_mean[i-1998,] = apply(HI_persub[6],2,mean)
HI_CCC_conf[i-1998,] = 1.96*(apply(HI_persub[6],2,sd)/sqrt(nrow(HI_persub[6])))}
colnames(HI_sp_mean)=colnames(HI_calc_sp)

#generate year column
year = 1999:2014

#Create dataframe with year, meanCCC, and CCC confidence intervals
HI_CCC_unc = data.frame(cbind(year,HI_CCC_mean,HI_CCC_conf))
colnames(HI_CCC_unc)=c("year","meanCCC","CCC_conf")

# Index out all species with < threshold % of total CCC in % of total CCC data
Threshold = 5
HI_per_sp_mean_dominant = HI_per_sp_mean[,!colMeans(HI_per_sp_mean)<Threshold]
HI_per_sp_mean_nondominant=data.matrix(rowSums(HI_per_sp_mean[,-which(colnames(HI_per_sp_mean) %in% c(colnames(HI_per_sp_mean_dominant)))]))
colnames(HI_per_sp_mean_nondominant) = c("Other")
HI_per_sp_year = data.matrix(cbind(year,HI_per_sp_mean_dominant,HI_per_sp_mean_nondominant))

# index dominant and nondominant species in CCC data using the threshold percent value
HI_sp_mean_dominant = HI_sp_mean[,which(colnames(HI_per_sp_mean) %in% c(colnames(HI_per_sp_mean_dominant)))]
HI_sp_mean_nondominant = data.matrix(rowSums(HI_sp_mean[,-which(colnames(HI_per_sp_mean) %in% c(colnames(HI_per_sp_mean_dominant)))]))
colnames(HI_sp_mean_nondominant) = c("Other")
HI_sp_year = data.matrix(cbind(year,HI_sp_mean_dominant,HI_sp_mean_nondominant))

#Melt the data to generate stacked bar plot in ggplot
HI_melt=melt(data.frame(HI_sp_year), id = c("year"))

#automate lme of species CCC vs. year with random slopes and intercepts by site
formulas = paste(noquote(colnames(HI_per_sp_mean_dominant)), noquote(replicate(length(colnames(HI_per_sp_mean_dominant)), "~ year")), sep = " ")
ctrl <- lmeControl(opt='optim');
HIlmefits = lapply(formulas, function(mm) summary(lme(as.formula(mm), random = ~year|site, corr = corCAR1(form = ~year|site), data=HI_PerCaCO3_Budget,control=ctrl,method = "ML")))
HIslopes = data.frame(lapply(HIlmefits, function(x) coef(x)[2,1]))
colnames(HIslopes) = colnames(HI_per_sp_mean_dominant)
HIslopes_err = data.frame(lapply(HIlmefits, function(x) coef(x)[2,2]))
colnames(HIslopes_err) = colnames(HIslopes)
HIslopes_df = data.frame(lapply(HIlmefits, function(x) coef(x)[2,3]))
colnames(HIslopes_df) = colnames(HIslopes)
HIslopes_t = data.frame(lapply(HIlmefits, function(x) coef(x)[2,4]))
colnames(HIslopes_t) = colnames(HIslopes)
HIslopes_p = data.frame(lapply(HIlmefits, function(x) coef(x)[2,5]))
colnames(HIslopes_p) = colnames(HIslopes)

#generate summary of slopes ± standard error and p-values for all dominant coral species
HI_lme_summary = rbind(colnames(HI_per_sp_mean_dominant),as.numeric(HIslopes),HIslopes_err,HIslopes_df,HIslopes_t,HIslopes_p)
HI_lme_summary = as.data.frame(t(HI_lme_summary))
colnames(HI_lme_summary) = c("species","slope","std_error","df","t","pvalue")
HI_lme_summary$slope=as.numeric(as.character(HI_lme_summary$slope))
HI_lme_summary$std_error=as.numeric(as.character(HI_lme_summary$std_error))
HI_lme_summary$df=as.numeric(as.character(HI_lme_summary$df))
HI_lme_summary$t=as.numeric(as.character(HI_lme_summary$t))
HI_lme_summary$pvalue=as.numeric(as.character(HI_lme_summary$pvalue))
HI_lme_summary

# generate coral cover ranging from 0 to 100% for simulated potential CCC by genera
cover = seq(0,100,by=0.01)

# Generate dataframe of potential CCC by species for dominant calcifying corals from the Main Hawaiian Islands
HI_montipora_capitata = cover/100*28.0
HI_montipora_patula = cover/100*28.0
HI_porites_compressa = cover/100*8.6
HI_porites_lobata = cover/100*14.6
HI_potential = data.frame(cbind(cover,HI_montipora_capitata,HI_montipora_patula,HI_porites_compressa,HI_porites_lobata))
