#Data from Powell_calcification script to use in the analysis of species contributions to CCC in the Florida Keys Reef Tract
FK_calc_sp
FK_taxa = FK_CREMP

# Remove all species with 0 CCC for all sites all years
FK_calc_sp = FK_calc_sp[,colSums(FK_calc_sp^2) !=0]

# Calculate %-total CCC by species for each site for each year
FK_Percalc_sp = matrix(nrow=nrow(FK_calc_sp),ncol=ncol(FK_calc_sp))
for (j in 1:nrow(FK_calc_sp))
{for (i in 1:ncol(FK_calc_sp))
{FK_Percalc_sp[j,i] = (FK_calc_sp[j,i]/FK_budget[j,])*100}}
colnames(FK_Percalc_sp)=colnames(FK_calc_sp)
FK_PerCaCO3_Budget = cbind(FK_taxa[c(1:4)],FK_PerCC,FK_CaCO3,FK_Percalc_sp)

# Calculate mean percent CCC for each species by year
FK_per_sp_mean = matrix(nrow=length(1996:2015),ncol=ncol(FK_Percalc_sp))
for (i in 1996:2015)
{FK_persub = subset(FK_PerCaCO3_Budget, FK_PerCaCO3_Budget$year == i)
FK_per_sp_mean[i-1995,] = apply(FK_persub[7:ncol(FK_PerCaCO3_Budget)],2,mean)}
colnames(FK_per_sp_mean)=colnames(FK_calc_sp)

# Create dataframe with site data, years, total CCC, and CCC per species
FK_CaCO3_Budget = cbind(FK_taxa[c(1:4)],FK_PerCC,FK_CaCO3,FK_calc_sp)
FKyear = FK_CaCO3_Budget$year
FKsite = FK_CaCO3_Budget$site
# Generate annual averages ± confidence intervals of CCC per species across all sites
FK_sp_mean = matrix(nrow=length(1996:2015),ncol=ncol(FK_calc_sp))
FK_CCC_mean = matrix(nrow=length(1996:2015),ncol=1)
FK_CCC_conf = matrix(nrow=length(1996:2015),ncol=1)
for (i in 1996:2015)
{FK_persub = subset(FK_CaCO3_Budget, FK_CaCO3_Budget$year == i)
FK_sp_mean[i-1995,] = apply(FK_persub[7:ncol(FK_CaCO3_Budget)],2,mean)
FK_CCC_mean[i-1995,] = apply(FK_persub[6],2,mean)
FK_CCC_conf[i-1995,] = 1.96*(apply(FK_persub[6],2,sd)/sqrt(nrow(FK_persub[6])))}
colnames(FK_sp_mean)=colnames(FK_calc_sp)

#generate year column
year = 1996:2015

#Create dataframe with year, meanCCC, and CCC confidence intervals
FK_CCC_unc = data.frame(cbind(year,FK_CCC_mean,FK_CCC_conf))
colnames(FK_CCC_unc)=c("year","meanCCC","CCC_conf")

# Index out all species with < threshold % of total CCC
Threshold = 5
FK_per_sp_mean_dominant = FK_per_sp_mean[,!colMeans(FK_per_sp_mean)<Threshold]
FK_per_sp_mean_nondominant=data.matrix(rowSums(FK_per_sp_mean[,-which(colnames(FK_per_sp_mean) %in% c(colnames(FK_per_sp_mean_dominant)))]))
colnames(FK_per_sp_mean_nondominant) = c("Other")
FK_per_sp_year = data.matrix(cbind(year,FK_per_sp_mean_dominant,FK_per_sp_mean_nondominant))

# index dominant and nondominant species using the threshold percent value
FK_sp_mean_dominant = FK_sp_mean[,which(colnames(FK_per_sp_mean) %in% c(colnames(FK_per_sp_mean_dominant)))]
FK_sp_mean_nondominant = data.matrix(rowSums(FK_sp_mean[,-which(colnames(FK_per_sp_mean) %in% c(colnames(FK_per_sp_mean_dominant)))]))
colnames(FK_sp_mean_nondominant) = c("Other")
FK_sp_year = data.matrix(cbind(year,FK_sp_mean_dominant,FK_sp_mean_nondominant))

#Melt the mean species CCC data to generate stacked bar plot in ggplot
FK_melt=melt(data.frame(FK_sp_year), id = c("year"))

#automate lme of species CCC vs. year with random slopes and intercepts by site
formulas = paste(noquote(colnames(FK_per_sp_mean_dominant)), noquote(replicate(length(colnames(FK_per_sp_mean_dominant)), "~ year")), sep = " ")
ctrl <- lmeControl(opt='optim');
FKlmefits = lapply(formulas, function(mm) summary(lme(as.formula(mm), random = ~year|site, corr = corCAR1(form = ~year|site), data=FK_PerCaCO3_Budget,control=ctrl,method = "ML")))
FKslopes = data.frame(lapply(FKlmefits, function(x) coef(x)[2,1]))
colnames(FKslopes) = colnames(FK_per_sp_mean_dominant)
FKslopes_err = data.frame(lapply(FKlmefits, function(x) coef(x)[2,2]))
colnames(FKslopes_err) = colnames(FKslopes)
FKslopes_df = data.frame(lapply(FKlmefits, function(x) coef(x)[2,3]))
colnames(FKslopes_df) = colnames(FKslopes)
FKslopes_t = data.frame(lapply(FKlmefits, function(x) coef(x)[2,4]))
colnames(FKslopes_t) = colnames(FKslopes)
FKslopes_p = data.frame(lapply(FKlmefits, function(x) coef(x)[2,5]))
colnames(FKslopes_p) = colnames(FKslopes)

#generate summary of slopes ± standard error and p-values for all dominant coral species
FK_lme_summary = rbind(colnames(FK_per_sp_mean_dominant),as.numeric(FKslopes),FKslopes_err,FKslopes_df,FKslopes_t,FKslopes_p)
FK_lme_summary = as.data.frame(t(FK_lme_summary))
colnames(FK_lme_summary) = c("species","slope","std_error","df","t","pvalue")
FK_lme_summary$slope=as.numeric(as.character(FK_lme_summary$slope))
FK_lme_summary$std_error=as.numeric(as.character(FK_lme_summary$std_error))
FK_lme_summary$df=as.numeric(as.character(FK_lme_summary$df))
FK_lme_summary$t=as.numeric(as.character(FK_lme_summary$t))
FK_lme_summary$pvalue=as.numeric(as.character(FK_lme_summary$pvalue))
FK_lme_summary

# generate coral cover ranging from 0 to 100% for simulated potential CCC by genera
cover = seq(0,100,by=0.01)

# Generate dataframe of potential CCC by species for dominant calcifying corals from the Florida Keys reef tract
FK_Acropora_palmata = cover/100*24
FK_Millepora_alcicornis= cover/100*28.1
FK_Montastraea_cavernosa = cover/100*10.8
FK_Orbicella_annularis_complex = cover/100*10.5
FK_Porites_astreoides = cover/100*6.3
FK_Siderastrea_siderea = cover/100*7.4
FK_potential = data.frame(cbind(cover,FK_Acropora_palmata,FK_Millepora_alcicornis,FK_Montastraea_cavernosa,FK_Orbicella_annularis_complex,FK_Porites_astreoides,FK_Siderastrea_siderea))

