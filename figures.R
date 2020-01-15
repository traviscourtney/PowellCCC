#Plot CCC by species/genera for each of the focal regions

HI_perCalcplot=
  ggplot()+geom_bar(data=HI_melt,aes(x=year,y=value,fill=variable),stat="identity",colour="black",size=0.25)+
  geom_errorbar(data= HI_CCC_unc,aes(x=year,ymax = meanCCC+CCC_conf, ymin=meanCCC-CCC_conf), width=0.2)+
  scale_x_continuous(limits=c(1995,2016),breaks=seq(1996,2015,1),expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0,12),breaks=seq(0,12,1),expand = c(0, 0))+
  ylab(expression(CCC~(kg~CaCO[3]~m^-2~yr^-1)))+
  xlab("")+
  ggtitle("(A) Main Hawaiian Islands")+
  scale_fill_manual("",values = c("montipora_capitata" = "#FFFFBD", "montipora_patula" = "#E6E68A", "porites_compressa"= "#B3E4FF", "porites_lobata" = "#6798BA", "Other" = "#D9D9D9"),
                    labels = c("Montipora capitata","Montipora patula","Porites compressa","Porites lobata","Other"))+
  guides(fill=guide_legend(ncol=3)) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

FK_perCalcplot=
  ggplot()+geom_bar(data=FK_melt,aes(x=year,y=value,fill=variable),stat="identity",colour="black",size=0.25)+
  geom_errorbar(data= FK_CCC_unc,aes(x=year,ymax = meanCCC+CCC_conf, ymin=meanCCC-CCC_conf), width=0.2)+
  scale_x_continuous(limits=c(1995,2016),breaks=seq(1996,2015,1),expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0,3),expand = c(0, 0))+
  xlab("")+ylab("")+ggtitle("(B) Florida Keys Reef Tract")+
  scale_fill_manual("", values = c("Acropora_palmata" = "#8DD3C7", "Millepora_alcicornis" = "#E49B49", "Montastraea_cavernosa"= "#B3DE69", "Orbicella_annularis_complex" = "#FCCDE5","Porites_astreoides" = "#4D7EA0", "Siderastrea_siderea"="#BC80BD","Other" = "#D9D9D9"),
                    labels = c("Acropora palmata","Millepora alcicornis","Montastraea cavernosa","Orbicella annularis complex","Porites astreoides","Siderastrea siderea","Other"))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

MO_perCalcplot=
  ggplot()+geom_bar(data=MO_melt,aes(x=year,y=value,fill=variable),stat="identity",colour="black",size=0.25)+
  geom_errorbar(data= MO_CCC_unc,aes(x=year,ymax = meanCCC+CCC_conf, ymin=meanCCC-CCC_conf), width=0.2)+
  scale_x_continuous(name = "Year",limits=c(1995,2016),breaks=seq(1996,2015,1),expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0,7),breaks=seq(0,7,1),expand = c(0, 0))+
  xlab("")+
  ylab(expression(CCC~(kg~CaCO[3]~m^-2~yr^-1)))+
  ggtitle("(C) Mo'orea")+
  scale_fill_manual("", values = c("Acropora" = "#8DD3C7", "Montipora" = "#FFFFB3", "Pavona"= "#BEBADA", "Pocillopora" = "#FB8072","Porites" = "#80B1D3", "Other" = "#D9D9D9"),
                    labels = c("Acropora","Montipora","Pavona","Pocillopora","Porites","Other"))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

SJ_perCalcplot=
  ggplot()+geom_bar(data=SJ_melt,aes(x=year,y=value,fill=variable),stat="identity",colour="black",size=0.25)+
  geom_errorbar(data= SJ_CCC_unc,aes(x=year,ymax = meanCCC+CCC_conf, ymin=meanCCC-CCC_conf), width=0.2)+
  scale_x_continuous(name = "Year",limits=c(1995,2016),breaks=seq(1996,2015,1),expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0,3),expand = c(0, 0))+
  ylab("")+
  ggtitle("(D) St. John")+
  scale_fill_manual("", values = c("Montastraea_cavernosa"= "#B3DE69", "Orbicella_annularis_complex" = "#FCCDE5","Porites_astreoides" = "#4D7EA0", "Porites_porites" = "#9ACBED", "Siderastrea_siderea"="#BC80BD","Other" = "#D9D9D9"),
                    labels = c("Montastraea cavernosa","Orbicella annularis complex","Porites astreoides","Porites porites","Siderastrea siderea","Other"))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

#CCC_by_taxa_figure
grid.arrange(HI_perCalcplot,FK_perCalcplot,MO_perCalcplot,SJ_perCalcplot,ncol=2)

#Plot CCC vs. % coral cover with potential calcification by dominant calcifiers

HIpotentialplot = ggplot(data=HI_CaCO3_Budget,aes(x=HI_PerCC,y=CCC))+
  xlab("% Coral Cover")+ylab(expression(CCC~(kg~CaCO[3]~m^-2~yr^-1)))+ggtitle("(A) Main Hawaiian Islands")+
  geom_line(data = HI_potential, aes(x = cover, y = HI_montipora_capitata), colour = "#FFFFBD",size=1.5)+
  geom_line(data = HI_potential, aes(x = cover, y = HI_montipora_patula), colour = "#E6E68A",size=1.5)+
  geom_line(data = HI_potential, aes(x = cover, y = HI_porites_compressa), colour = "#B3E4FF",size=1.5)+
  geom_line(data = HI_potential, aes(x = cover, y = HI_porites_lobata), colour = "#6798BA",size=1.5)+
  geom_point(shape=1,size=3)+
  scale_y_continuous(limits = c(0, 30),expand = c(0, 0))+scale_x_continuous(limits=c(0, 100), expand = c(0, 0))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

FKpotentialplot = ggplot(data=FK_CaCO3_Budget,aes(x=FK_PerCC,y=CCC))+
  xlab("")+ylab("")+ggtitle("(C) Florida Keys Reef Tract")+
  geom_line(data = FK_potential, aes(x = cover, y = FK_Acropora_palmata),colour = "#8DD3C7",size=1.5)+
  geom_line(data = FK_potential, aes(x = cover, y = FK_Millepora_alcicornis), colour = "#E49B49",size=1.5)+
  geom_line(data = FK_potential, aes(x = cover, y = FK_Montastraea_cavernosa), colour = "#B3DE69",size=1.5)+
  geom_line(data = FK_potential, aes(x = cover, y = FK_Orbicella_annularis_complex), colour = "#FCCDE5",size=1.5)+
  geom_line(data = FK_potential, aes(x = cover, y = FK_Porites_astreoides), colour = "#4D7EA0",size=1.5)+
  geom_line(data = FK_potential, aes(x = cover, y = FK_Siderastrea_siderea), colour = "#BC80BD",size=1.5)+
  geom_point(shape=1,size=3)+
  scale_y_continuous(limits = c(0, 15),expand = c(0, 0))+scale_x_continuous(limits=c(0, 50), expand = c(0, 0))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

MOpotentialplot = ggplot(data=MO_CaCO3_Budget,aes(x=MO_PerCC,y=CCC))+
  xlab("% Coral Cover")+ylab(expression(CCC~(kg~CaCO[3]~m^-2~yr^-1)))+ggtitle("(B) Mo'orea")+
  geom_line(data = MO_potential, aes(x = cover, y = MO_Acropora), colour = "#8DD3C7",size=1.5)+
  geom_line(data = MO_potential, aes(x = cover, y = MO_Montipora), colour = "#FFFFB3",size=1.5)+
  geom_line(data = MO_potential, aes(x = cover, y = MO_Pavona), colour = "#BEBADA",size=1.5)+
  geom_line(data = MO_potential, aes(x = cover, y = MO_Pocillopora), colour = "#FB8072",size=1.5)+
  geom_line(data = MO_potential, aes(x = cover, y = MO_Porites), colour = "#80B1D3",size=1.5)+
  geom_point(shape=1,size=3)+
  scale_y_continuous(limits = c(0, 30),expand = c(0, 0))+scale_x_continuous(limits=c(0, 100), expand = c(0, 0))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

SJpotentialplot = ggplot()+
  xlab("% Coral Cover")+ylab("")+ggtitle("(D) St. John")+
  geom_line(data = SJ_potential, aes(x = cover, y = SJ_Montastrea_cavernosa),color="#B3DE69",size=1.5)+
  geom_line(data = SJ_potential, aes(x = cover, y = SJ_Orbicella_annularis_complex),color="#FCCDE5",size=1.5)+
  geom_line(data = SJ_potential, aes(x = cover, y = SJ_Porites_astreoides),color="#4D7EA0",size=1.5)+
  geom_line(data = SJ_potential, aes(x = cover, y = SJ_Porites_porites),color="#9ACBED",size=1.5)+
  geom_line(data = SJ_potential, aes(x = cover, y = SJ_Siderastrea_siderea),color="#BC80BD",size=1.5)+
  geom_point(data=SJcsun_CaCO3_Budget,aes(x=SJcsun_PerCC,y=CCC),shape=1,size=3)+
  geom_point(data=SJnps_CaCO3_Budget,aes(x=SJnps_PerCC,y=CCC),shape=1,size=3)+
  scale_y_continuous(limits = c(0, 15),expand = c(0, 0))+scale_x_continuous(limits=c(0, 50), expand = c(0, 0))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

#CCC_vs_coral_cover_plot
grid.arrange(HIpotentialplot,FKpotentialplot,MOpotentialplot,SJpotentialplot,ncol=2)

# Plot CCC vs. coral cover slopes and 95% confidence intervals for each focal region
CCslopeplot=
  ggplot(CCslopes, aes(Regions,Slope)) + #geom_point() + 
  geom_pointrange(aes(ymin=Slope-Conf.Int, ymax=Slope+Conf.Int),size=2) + 
  geom_pointrange(aes(ymin=Slope-Std.Error, ymax=Slope+Std.Error), size = 2.25, colour = "gray")+
  xlab("Region")+
  scale_y_continuous(limits = c(0, 0.25),expand = c(0, 0))+
  ylab(expression(Slope~(kg~CaCO[3]~m^-2~yr^-1~'%'~coral~cover^-1)))+
  theme_bw(base_size=22)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

#Plot change in percent contribution of each taxa to annual CCC
HI_species_year_plot=
  ggplot(HI_lme_summary,aes(species,slope)) + geom_point()+
  geom_hline(yintercept=0, linetype="dashed", colour = "black")+
  geom_pointrange(aes(ymin=slope-1.96*std_error, ymax=slope+1.96*std_error),size=1.25) + 
  geom_pointrange(aes(ymin=slope-std_error, ymax=slope+std_error), colour = "gray",size=1.25)+
  xlab("")+ylab(expression('%'~of~CCC~vs.~Year~(~'%'~CCC~yr^-1)))+
  scale_x_discrete(labels = c("Montipora capitata","Montipora patula","Porites compressa","Porites lobata"))+
  scale_y_continuous(limits = c(-3,2))+
  ggtitle("(A) Main Hawaiian Islands")+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

FK_species_year_plot=
  ggplot(FK_lme_summary,aes(species,slope)) + geom_point()+
  geom_hline(yintercept=0, linetype="dashed", colour = "black")+
  geom_pointrange(aes(ymin=slope-1.96*std_error, ymax=slope+1.96*std_error),size=1.25) + 
  geom_pointrange(aes(ymin=slope-std_error, ymax=slope+std_error), colour = "gray",size=1.25)+
  xlab("")+ylab("")+
  scale_x_discrete(labels = c("Acropora palmata","Millepora alcicornis","Montastraea cavernosa","Orbicella annularis complex","Porites astreoides","Siderastrea siderea"))+
  scale_y_continuous(limits = c(-3,2))+
  ggtitle("(B) Florida Keys Reef Tract")+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

MO_genus_year_plot=
  ggplot(MO_lme_summary,aes(genera,slope)) + geom_point()+
  geom_hline(yintercept=0, linetype="dashed", colour = "black")+
  geom_pointrange(aes(ymin=slope-1.96*std_error, ymax=slope+1.96*std_error),size=1.25) + 
  geom_pointrange(aes(ymin=slope-std_error, ymax=slope+std_error), colour = "gray",size=1.25)+
  xlab("")+ylab(expression('%'~of~CCC~vs.~Year~(~'%'~CCC~yr^-1)))+
  scale_y_continuous(limits = c(-3,2))+
  ggtitle("(C) Mo'orea")+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

SJ_species_year_plot=
  ggplot(SJ_lme_summary,aes(species,slope)) + geom_point()+
  geom_hline(yintercept=0, linetype="dashed", colour = "black")+
  geom_pointrange(aes(ymin=slope-1.96*std_error, ymax=slope+1.96*std_error),size=1.25) + 
  geom_pointrange(aes(ymin=slope-std_error, ymax=slope+std_error), colour = "gray",size=1.25)+
  xlab("")+ylab("")+
  scale_y_continuous(limits = c(-3,2))+
  scale_x_discrete(labels = c("Montastraea cavernosa","Orbicella annularis complex","Porites astreoides","Porites porites","Siderastrea siderea"))+
  ggtitle("(D) St. John")+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

#Percent_CCC_change_year_plot
grid.arrange(HI_species_year_plot,FK_species_year_plot,MO_genus_year_plot,SJ_species_year_plot,ncol=2)


