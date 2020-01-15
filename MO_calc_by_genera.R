#Data from Powell_calcification script to use in the analysis of species contributions to CCC in Mo'orea
MO_calc_sp
MO_taxa

#Group species by genus
Porites=rowSums(MO_calc_sp[,23:26])
MO_calc_gen = data.matrix(cbind(MO_calc_sp[,1:22],Porites,MO_calc_sp[,27:30]))

# Remove all genera with 0 CCC for all sites all years
MO_calc_gen = MO_calc_gen[,colSums(MO_calc_gen^2) !=0]

# Calculate %-total CCC by genera for each site for each year
MO_Percalc_gen = matrix(nrow=nrow(MO_calc_gen),ncol=ncol(MO_calc_gen))
for (j in 1:nrow(MO_calc_gen))
{for (i in 1:ncol(MO_calc_gen))
{MO_Percalc_gen[j,i] = (MO_calc_gen[j,i]/MO_budget[j,])*100}}
colnames(MO_Percalc_gen)=colnames(MO_calc_gen)
MO_PerCaCO3_Budget = cbind(MO_taxa[c(1:4)],MO_PerCC,MO_CaCO3,MO_Percalc_gen)

# Calculate mean ± sd percent CCC for each genus by year
MO_per_gen_mean = matrix(nrow=length(2005:2015),ncol=ncol(MO_Percalc_gen))
for (i in 2005:2015)
{MO_persub = subset(MO_PerCaCO3_Budget, MO_PerCaCO3_Budget$year == i)
MO_per_gen_mean[i-2004,] = apply(MO_persub[7:ncol(MO_PerCaCO3_Budget)],2,mean)}
colnames(MO_per_gen_mean)=colnames(MO_calc_gen)

# Create dataframe with site data, years, total CCC, and CCC per genus
MO_CaCO3_Budget = cbind(MO_taxa[c(1:4)],MO_PerCC,MO_CaCO3,MO_calc_gen)
MOyear = MO_CaCO3_Budget$year
MOsite = MO_CaCO3_Budget$site
# Generate annual averages ± confidence intervals of CCC per genus across all sites
MO_gen_mean = matrix(nrow=length(2005:2015),ncol=ncol(MO_calc_gen))
MO_CCC_mean = matrix(nrow=length(2005:2015),ncol=1)
MO_CCC_conf = matrix(nrow=length(2005:2015),ncol=1)
for (i in 2005:2015)
{MO_persub = subset(MO_CaCO3_Budget, MO_CaCO3_Budget$year == i)
MO_gen_mean[i-2004,] = apply(MO_persub[7:ncol(MO_CaCO3_Budget)],2,mean)
MO_CCC_mean[i-2004,] = apply(MO_persub[6],2,mean)
MO_CCC_conf[i-2004,] = 1.96*(apply(MO_persub[6],2,sd)/sqrt(nrow(MO_persub[6])))}
colnames(MO_gen_mean)=colnames(MO_calc_gen)

#generate year column
year = 2005:2015

#Create dataframe with year, meanCCC, and CCC confidence intervals
MO_CCC_unc = data.frame(cbind(year,MO_CCC_mean,MO_CCC_conf))
colnames(MO_CCC_unc)=c("year","meanCCC","CCC_conf")

# Index out all genera with < threshold % of total CCC
Threshold = 5
MO_per_gen_mean_dominant = MO_per_gen_mean[,!colMeans(MO_per_gen_mean)<Threshold]
MO_per_gen_mean_nondominant=data.matrix(rowSums(MO_per_gen_mean[,-which(colnames(MO_per_gen_mean) %in% c(colnames(MO_per_gen_mean_dominant)))]))
colnames(MO_per_gen_mean_nondominant) = c("Other")
MO_per_gen_year = data.matrix(cbind(year,MO_per_gen_mean_dominant,MO_per_gen_mean_nondominant))

# index dominant and nondominant species using the threshold percent value
MO_gen_mean_dominant = MO_gen_mean[,which(colnames(MO_per_gen_mean) %in% c(colnames(MO_per_gen_mean_dominant)))]
MO_gen_mean_nondominant = data.matrix(rowSums(MO_gen_mean[,-which(colnames(MO_per_gen_mean) %in% c(colnames(MO_per_gen_mean_dominant)))]))
colnames(MO_gen_mean_nondominant) = c("Other")
MO_gen_year = data.matrix(cbind(year,MO_gen_mean_dominant,MO_gen_mean_nondominant))

#Melt the data to generate stacked bar plot in ggplot
MO_melt=melt(data.frame(MO_gen_year), id = c("year"))

#automate lme of genera CCC vs. year with random slopes and intercepts by site
formulas = paste(noquote(colnames(MO_per_gen_mean_dominant)), noquote(replicate(length(colnames(MO_per_gen_mean_dominant)), "~ year")), sep = " ")
ctrl <- lmeControl(opt='optim');
MOlmefits = lapply(formulas, function(mm) summary(lme(as.formula(mm), random = ~year|site, corr = corCAR1(form = ~year|site), data=MO_PerCaCO3_Budget,control=ctrl,method = "ML")))
MOslopes = data.frame(lapply(MOlmefits, function(x) coef(x)[2,1]))
colnames(MOslopes) = colnames(MO_per_gen_mean_dominant)
MOslopes_err = data.frame(lapply(MOlmefits, function(x) coef(x)[2,2]))
colnames(MOslopes_err) = colnames(MOslopes)
MOslopes_df = data.frame(lapply(MOlmefits, function(x) coef(x)[2,3]))
colnames(MOslopes_df) = colnames(MOslopes)
MOslopes_t = data.frame(lapply(MOlmefits, function(x) coef(x)[2,4]))
colnames(MOslopes_t) = colnames(MOslopes)
MOslopes_p = data.frame(lapply(MOlmefits, function(x) coef(x)[2,5]))
colnames(MOslopes_p) = colnames(MOslopes)

#generate summary of slopes ± standard error and p-values for all dominant coral genera 
MO_lme_summary = rbind(colnames(MO_per_gen_mean_dominant),as.numeric(MOslopes),MOslopes_err,MOslopes_df,MOslopes_t,MOslopes_p)
MO_lme_summary = as.data.frame(t(MO_lme_summary))
colnames(MO_lme_summary) = c("genera","slope","std_error","df","t","pvalue")
MO_lme_summary$slope=as.numeric(as.character(MO_lme_summary$slope))
MO_lme_summary$std_error=as.numeric(as.character(MO_lme_summary$std_error))
MO_lme_summary$df=as.numeric(as.character(MO_lme_summary$df))
MO_lme_summary$t=as.numeric(as.character(MO_lme_summary$t))
MO_lme_summary$pvalue=as.numeric(as.character(MO_lme_summary$pvalue))
MO_lme_summary

# generate coral cover ranging from 0 to 100% for simulated potential CCC by genera
cover = seq(0,100,by=0.01)

# Generate dataframe of potential CCC by genera for the dominant calcifying corals from Mo'orea
MO_Acropora = cover/100*20.6
MO_Montipora = cover/100*25.6
MO_Pavona = cover/100*19.7
MO_Pocillopora = cover/100*8.0
MO_Porites = cover/100*14.2
MO_potential = data.frame(cbind(cover,MO_Acropora,MO_Montipora,MO_Pavona,MO_Pocillopora,MO_Porites))
