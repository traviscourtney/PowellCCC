######################################################################
##’ @title Functions to sum coral reef calcification by corals
##’
##’ @author Travis A Courtney
##’ @contact traviscourtney@gmail.com
##’ @date 2017-06–29
##’ @reference Guest et al Generation of Coral Calcification Capacity from Taxa and Rate data
##’ @log Add a log here
######################################################################

#load libraries
library(ggplot2)

# Import all taxa cover and calcification rates files
setwd("Data")
HiCalcRates <- read.csv("Rates_MHI.csv")
Mhi_CRAMP_taxa <- read.csv("Taxa_MHI.csv")
MO_taxa = read.csv("Taxa_MO.csv")
MO_calc_rates = read.csv("Rates_MO_2.csv")
FK_calc_rates = read.csv("Rates_FK.csv")
FK_CREMP = read.csv("Taxa_FK.csv")
SJnps_calc_rates = read.csv("Rates_SJ_nps.csv")
SJnps_taxa = read.csv("Guest_data_StJohn_NPS_taxa.csv")
SJcsun_calc_rates = read.csv("Rates_SJ_csun.csv")
SJcsun_taxa = read.csv("Guest_data_StJohn_CSU_taxa.csv")
setwd('..')

# Main Hawaiian Islands
#Convert data to matrices
HI_cover <- data.matrix(Mhi_CRAMP_taxa[c(5:31)])
HI_rates <- data.matrix(HiCalcRates[c(2)])

# Calculate calcification by each species by site by year
HI_calc_sp = matrix(nrow=nrow(HI_cover),ncol=nrow(HI_rates))
for (i in 1:length(HI_rates))
{HI_calc_sp[,i] = HI_rates[i,]*(HI_cover[,i]/100)}
colnames(HI_calc_sp)=colnames(HI_cover)

#Sum CaCO3 production by species to get CPCC by site by year
HI_budget = data.matrix(rowSums(HI_calc_sp))
HI_CaCO3 = data.matrix(round(HI_budget,digits = 1))
colnames(HI_CaCO3) = c("CCC")

# Sum %-cover by taxa by year to get %-hard coral cover at Moorea
HI_PerCC = data.matrix(round(rowSums(HI_cover), digits = 1))

# Write CCC data to .csv file
Mhi_CRAMP <- Mhi_CRAMP_taxa[c(1:4)]
HI_CaCO3_Budget = cbind(Mhi_CRAMP,HI_PerCC,HI_CaCO3)

# Plot %-Coral Cover against CCC for all sites/years
HIplot = ggplot(data=HI_CaCO3_Budget,aes(x=HI_PerCC,y=CCC))+
  geom_point(shape=1)+theme_bw()+xlab("% Coral Cover")+ylab("CCC (G)")+ggtitle("Hawaii CRAMP")+
  scale_y_continuous(limits = c(0, 25))+scale_x_continuous(limits = c(0, 100))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Moorea
# Create data matrices
MO_cover = data.matrix(MO_taxa[c(5:34)])
MO_rates = data.matrix(MO_calc_rates[c(2)])

# Calculate CaCO3 production by species
MO_calc_sp = matrix(nrow=nrow(MO_cover),ncol=nrow(MO_rates))
for (i in 1:length(MO_rates))
{MO_calc_sp[,i] = MO_rates[i,]*(MO_cover[,i]/100)}
colnames(MO_calc_sp)=colnames(MO_cover)

# Sum CaCO3 production by species to get CPCC per site by year
MO_budget = data.matrix(rowSums(MO_calc_sp))
MO_CaCO3 = data.matrix(round(MO_budget, digits = 1))
colnames(MO_CaCO3) = c("CCC")

# Sum %-cover by taxa by year to get %-hard coral cover at Moorea
MO_PerCC = data.matrix(round(rowSums(MO_cover),digits = 1))

# Write Moorea CPCC to .csv
MO_CaCO3_Budget = cbind(MO_taxa[c(1:4)],MO_PerCC,MO_CaCO3)

# Plot Moorea %-coral cover against CCC
MOplot = ggplot(data=MO_CaCO3_Budget,aes(x=MO_PerCC,y=CCC))+
  geom_point(shape=1)+theme_bw()+xlab("% Coral Cover")+ylab("CCC (G)")+ggtitle("Moorea LTER")+
  scale_y_continuous(limits = c(0, 25))+scale_x_continuous(limits = c(0, 100))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Florida Keys
# Create data matrices
FK_cover = data.matrix(FK_CREMP[c(5:44)])
FK_rates = data.matrix(FK_calc_rates[c(2)])

# Calculate CaCO3 production by species
FK_calc_sp = matrix(nrow=nrow(FK_cover),ncol=nrow(FK_rates))
for (i in 1:length(FK_rates))
{FK_calc_sp[,i] = FK_rates[i,]*(FK_cover[,i]/100)}
colnames(FK_calc_sp)=colnames(FK_cover)

# Sum CaCO3 production by species to get CCC by site by year
FK_budget = data.matrix(rowSums(FK_calc_sp))
FK_CaCO3 = data.matrix(round(FK_budget, digits = 1))
colnames(FK_CaCO3) = c("CCC")

# Sum %-cover by taxa to get %-Coral Cover for Florida (includes Millepora)
FK_PerCC = data.matrix(round(rowSums(FK_cover),digits=1))

# Write CCC data to .csv
FK_CaCO3_Budget = cbind(FK_CREMP[c(1:4)],FK_PerCC,FK_CaCO3)

# Plot
FKplot = ggplot(data=FK_CaCO3_Budget,aes(x=FK_PerCC,y=CCC))+
  geom_point(shape=1)+theme_bw()+xlab("% Coral Cover")+ylab("CCC (G)")+ggtitle("Florida CREMP")+
  scale_y_continuous(limits = c(0, 25))+scale_x_continuous(limits = c(0, 100))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#St John NPS
#Convert data to matrices
SJnps_cover = data.matrix(SJnps_taxa[c(5:72)])
SJnps_rates = data.matrix(SJnps_calc_rates[c(2)])

# Calculate CaCO3 production by species
SJnps_calc_sp = matrix(nrow=nrow(SJnps_cover),ncol=nrow(SJnps_rates))
for (i in 1:length(SJnps_rates))
{SJnps_calc_sp[,i] = SJnps_rates[i,]*(SJnps_cover[,i]/100)}
colnames(SJnps_calc_sp)=t(SJnps_calc_rates[c(1)])

# Sum CaCO3 production by species to get CCC by site by year
SJnps_budget = data.matrix(rowSums(SJnps_calc_sp))
SJnps_CaCO3 = data.matrix(round(SJnps_budget, digits = 1))
colnames(SJnps_CaCO3) = c("CCC")

# Sum %-cover by taxa to get %-Coral Cover for St John NPS (includes Millepora)
SJnps_PerCC = data.matrix(round(rowSums(SJnps_cover),digits=1))

# Write CCC data to .csv
SJnps_CaCO3_Budget = cbind(SJnps_taxa[c(1:4)],SJnps_PerCC,SJnps_CaCO3)

# Plot
SJnpsplot = ggplot(data=SJnps_CaCO3_Budget,aes(x=SJnps_PerCC,y=CCC))+
  geom_point(shape=1)+theme_bw()+xlab("% Coral Cover")+ylab("CCC (G)")+ggtitle("St. John NPS")+
  scale_y_continuous(limits = c(0, 25))+scale_x_continuous(limits = c(0, 100))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# St. John CSUN
# Convert data to matrices

SJcsun_cover = data.matrix(SJcsun_taxa[c(5:35)])
SJcsun_rates = data.matrix(SJcsun_calc_rates[c(2)])

# Calculate CaCO3 production by species
SJcsun_calc_sp = matrix(nrow=nrow(SJcsun_cover),ncol=nrow(SJcsun_rates))
for (i in 1:length(SJcsun_rates))
{SJcsun_calc_sp[,i] = SJcsun_rates[i,]*(SJcsun_cover[,i]/100)}
colnames(SJcsun_calc_sp)=colnames(SJcsun_cover)

# Sum CaCO3 production by species to get CCC by site by year
SJcsun_budget = data.matrix(rowSums(SJcsun_calc_sp))
SJcsun_CaCO3 = data.matrix(round(SJcsun_budget, digits = 1))
colnames(SJcsun_CaCO3) = c("CCC")

# Sum %-cover by taxa to get %-Coral Cover for St John csun (includes Millepora)
SJcsun_PerCC = data.matrix(round(rowSums(SJcsun_cover),digits=1))

# Write CCC data to .csv
SJcsun_CaCO3_Budget = cbind(SJcsun_taxa[c(1:4)],SJcsun_PerCC,SJcsun_CaCO3)

# Plot
SJcsunplot = ggplot(data=SJcsun_CaCO3_Budget,aes(x=SJcsun_PerCC,y=CCC))+
  geom_point(shape=1)+theme_bw()+xlab("% Coral Cover")+ylab("CCC (G)")+ggtitle("St. John CSUN")+
  scale_y_continuous(limits = c(0, 25))+scale_x_continuous(limits = c(0, 100))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
