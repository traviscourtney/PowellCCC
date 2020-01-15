#combine SJcsun and SJnps CCC before running CCC vs CC model
colnames(SJcsun_CaCO3_Budget)=colnames(SJnps_CaCO3_Budget)
SJ_CaCO3_Budget = rbind(SJnps_CaCO3_Budget,SJcsun_CaCO3_Budget)

#set lmeControl to aid in model convergence
ctrl = lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt="optim")

#run CCC vs CC lme with random intercepts by site for each focal region
HI_model = lme(CCC~HI_PerCC, random=~HI_PerCC|site,data=HI_CaCO3_Budget,method="ML",control=ctrl)
FK_model = lme(CCC~FK_PerCC, random=~FK_PerCC|site,data=FK_CaCO3_Budget,method="ML",control=ctrl)
SJ_model = lme(CCC~SJnps_PerCC, random=~SJnps_PerCC|Site_name,data=SJ_CaCO3_Budget,method="ML",control=ctrl)
MO_model = lme(CCC~MO_PerCC, random=~MO_PerCC|site,data=MO_CaCO3_Budget,method="ML",control=ctrl)

# summarize lmer outputs into dataframes
HI_output = as.data.frame(coef(summary(HI_model)))
MO_output = as.data.frame(coef(summary(MO_model)))
FK_output = as.data.frame(coef(summary(FK_model)))
SJ_output = as.data.frame(coef(summary(SJ_model)))

# extract slopes and standard errors for lmes of each focal region
Values = cbind(HI_output$Value,MO_output$Value,FK_output$Value,SJ_output$Value)
Std.Errors = cbind(HI_output$Std.Error,MO_output$Std.Error,FK_output$Std.Error,SJ_output$Std.Error)
colnames(Values)=c('Hawaii','Moorea','Florida','St. John')
colnames(Std.Errors)=c('Hawaii','Moorea','Florida','St. John')

# combine slopes and standard errors into single table
Regions = c('Main Hawaiian Islands','Moorea','Florida Keys','St. John')
CCslopes = as.data.frame(cbind(Values[2,],Std.Errors[2,],1.96*Std.Errors[2,]))
colnames(CCslopes)=c('Slope','Std.Error','Conf.Int')

