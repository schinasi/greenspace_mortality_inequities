setwd ('W:/UHC/Schinasi_Infant_Mortality/AHA/AHA_Data')

memory.limit(size=500000) #increase memory so that data can be read in

##################
library(tableone)
library(haven)
library(pscl)
library(MASS)
library(scales)
library(glm2)

##################
load("W:/UHC/Schinasi_Infant_Mortality/AHA/equigenic/equigenic_dataprep_workspace.RData")

#rescaled to be between 0 and 1

# load functions

source('C:/Users/lhs36/Dropbox/Leah Work/AHA WORKING FOLDER/mortality_disparities_equigenic/2022feb16_newstandardfunctions_alter_covs.R')

datan<-data

############
date<-'2022feb28'
adjust<-"gender_age"
covs<-c("gender", "newage")

###########
#folder for output
setwd('C:/Users/lhs36/Dropbox/Leah Work/AHA WORKING FOLDER/mortality_disparities_equigenic/results/feb282022')

# RUN MAIN MODELS with minimal adjustment: Gender and age
outcome<-"ndeath"

pov<-pleaserun('s_pov', 'ndeath', cov=covs)
race<-pleaserun('s_blk', 'ndeath',cov=covs)
ice<-pleaserun('s_iceblk', 'ndeath', cov=covs)
main_allcause<-rbind(pov, race, ice)
write.csv(main_allcause, paste(date,outcome, "_" , adjust, ".csv"))
          
# RUN MAIN MODELS. MAke sure to run the function 

outcome<-"ncvddeath"

pov<-pleaserun('s_pov', 'ncvddeath', cov=c("gender", "newage"))
race<-pleaserun('s_blk', 'ncvddeath', cov=c("gender", "newage"))
ice<-pleaserun('s_iceblk', 'ncvddeath', cov=covs)

main_cvd<-rbind(pov, race, ice)
write.csv(main_cvd, paste(date,outcome, "_" , adjust, ".csv"))

#####################################################
### Interaction models ## 
### Park race all cause mortality

covs<-c("gender", "newage")

parkraceall_1<-pleaserunint('s_blk', 'ndeath', 
                                'parkcatn',
                                0, 1,
                                2, 
                                'min-q1 park',covs )

parkraceall_2<-pleaserunint('s_blk', 'ndeath', 
                                'parkcatn',
                                1, 0,
                                2,  
                                'q1-q2 park', covs)

parkraceall_3<-pleaserunint('s_blk', 'ndeath', 
                                'parkcatn',
                                2, 1,
                                0,  
                                'q2-q3 park', covs)

park_race_all<-rbind(parkraceall_1, parkraceall_2, parkraceall_3)

#park poverty all cause

parkpovertyall_1<-pleaserunint('s_pov', 'ndeath', 
                                   'parkcatn',
                                   0, 1,
                                   2, 
                                   'min-q1 park', covs)

parkpovertyall_2<-pleaserunint('s_pov', 'ndeath', 
                                   'parkcatn',
                                   1, 0,
                                   2, 
                                   'q1-q2 park', covs)

parkpovertyall_3<-pleaserunint('s_pov', 'ndeath', 
                                   'parkcatn',
                                   2, 1,
                                   0,  
                                   'q2-q3 park', covs)

park_poverty_all<-rbind(parkpovertyall_1, parkpovertyall_2, parkpovertyall_3)


#park ice all
parkiceblkall_1<-pleaserunint('s_iceblk', 'ndeath', 
                               'parkcatn',
                               0, 1,
                               2, 
                               'min-q1 park', covs)

parkiceblkall_2<-pleaserunint('s_iceblk', 'ndeath', 
                               'parkcatn',
                               1, 0,
                               2, 
                               'q1-q2 park', covs)

parkiceblkall_3<-pleaserunint('s_iceblk', 'ndeath', 
                               'parkcatn',
                               2, 1,
                               0,  
                               'q2-q3 park', covs)

park_iceblk_all<-rbind(parkiceblkall_1, parkiceblkall_2, parkiceblkall_3)

#### Trees
treeraceall_1<-pleaserunint('s_blk', 'ndeath', 
                                'treecatn',
                                0, 1,
                                2, 
                                'min-q1 tree', covs)

treeraceall_2<-pleaserunint('s_blk', 'ndeath', 
                                'treecatn',
                                1, 0,
                                2, 
                                'q1-q2 tree', covs)

treeraceall_3<-pleaserunint('s_blk', 'ndeath', 
                                'treecatn',
                                2, 1,
                                0, 
                                'q2-q3 tree', covs)


tree_race_all<-rbind(treeraceall_1, treeraceall_2, treeraceall_3)

#tree poverty all cause

treepovertyall_1<-pleaserunint('s_pov', 'ndeath', 
                                   'treecatn',
                                   0, 1,
                                   2,  
                                   'min-q1 tree', covs)

treepovertyall_2<-pleaserunint('s_pov', 'ndeath', 
                                   'treecatn',
                                   1, 0,
                                   2,  
                                   'q1-q2 tree', covs)

treepovertyall_3<-pleaserunint('s_pov', 'ndeath', 
                                   'treecatn',
                                   2, 1,
                                   0,  
                                   'q2-q3 tree', covs)



tree_poverty_all<-rbind(treepovertyall_1, treepovertyall_2, treepovertyall_3)

# tree ice

treeiceblkall_1<-pleaserunint('s_iceblk', 'ndeath', 
                              'treecatn',
                              0, 1,
                              2, 
                              'min-q1 tree', covs)

treeiceblkall_2<-pleaserunint('s_iceblk', 'ndeath', 
                              'treecatn',
                              1, 0,
                              2, 
                              'q1-q2 tree', covs)

treeiceblkall_3<-pleaserunint('s_iceblk', 'ndeath', 
                              'treecatn',
                              2, 1,
                              0,  
                              'q2-q3 tree', covs)

tree_iceblk_all<-rbind(treeiceblkall_1, treeiceblkall_2, treeiceblkall_3)

#################### CVD MODELS ##############################################################

parkracecvd_1<-pleaserunint('s_blk', 'ncvddeath', 
                                'parkcatn',
                                0, 1,
                                2, 
                                'min-q1 park', covs)

parkracecvd_2<-pleaserunint('s_blk', 'ncvddeath', 
                                'parkcatn',
                                1, 0,
                                2, 
                                'q1-q2 park', covs)

parkracecvd_3<-pleaserunint('s_blk', 'ncvddeath', 
                                'parkcatn',
                                2, 1,
                                0, 
                                'q2-q3 park', covs)


park_race_cvd<-rbind(parkracecvd_1, parkracecvd_2, parkracecvd_3)

#park poverty cvd cause

parkpovertycvd_1<-pleaserunint('s_pov', 'ncvddeath', 
                                   'parkcatn',
                                   0, 1,
                                   2, 
                                   'min-q1 park', covs)

parkpovertycvd_2<-pleaserunint('s_pov', 'ncvddeath', 
                                   'parkcatn',
                                   1, 0,
                                   2,  
                                   'q1-q2 park', covs)

parkpovertycvd_3<-pleaserunint('s_pov', 'ncvddeath', 
                                   'parkcatn',
                                   2, 1,
                                   0, 
                                   'q2-q3 park', covs)


park_poverty_cvd<-rbind(parkpovertycvd_1, parkpovertycvd_2, parkpovertycvd_3)

#park ice cvd

parkiceblkcvd_1<-pleaserunint('s_iceblk', 'ncvddeath', 
                              'parkcatn',
                              0, 1,
                              2, 
                              'min-q1 park', covs)

parkiceblkcvd_2<-pleaserunint('s_iceblk', 'ncvddeath', 
                              'parkcatn',
                              1, 0,
                              2, 
                              'q1-q2 park', covs)

parkiceblkcvd_3<-pleaserunint('s_iceblk', 'ncvddeath', 
                              'parkcatn',
                              2, 1,
                              0,  
                              'q2-q3 park', covs)

park_iceblk_cvd<-rbind(parkiceblkcvd_1, parkiceblkcvd_2, parkiceblkcvd_3)


###########################
#### Trees
treeracecvd_1<-pleaserunint('s_blk', 'ncvddeath', 
                                'treecatn',
                                0, 1,
                                2, 
                                'min-q1 tree', covs)

treeracecvd_2<-pleaserunint('s_blk', 'ncvddeath', 
                                'treecatn',
                                1, 0,
                                2, 
                                'q1-q2 tree', covs)

treeracecvd_3<-pleaserunint('s_blk', 'ncvddeath', 
                                'treecatn',
                                2, 1,
                                0, 
                                'q2-q3 tree', covs)


tree_race_cvd<-rbind(treeracecvd_1, treeracecvd_2, treeracecvd_3)

#tree poverty cvd cause

treepovertycvd_1<-pleaserunint('s_pov', 'ncvddeath', 
                                   'treecatn',
                                   0, 1,
                                   2, 
                                   'min-q1 tree', covs)

treepovertycvd_2<-pleaserunint('s_pov', 'ncvddeath', 
                                   'treecatn',
                                   1, 0,
                                   2, 
                                   'q1-q2 tree', covs)

treepovertycvd_3<-pleaserunint('s_pov', 'ncvddeath', 
                                   'treecatn',
                                   2, 1,
                                   0,  
                                   'q2-q3 tree', covs)


tree_poverty_cvd<-rbind(treepovertycvd_1, treepovertycvd_2, treepovertycvd_3)

# tree ice cvd



treeiceblkcvd_1<-pleaserunint('s_iceblk', 'ncvddeath', 
                              'treecatn',
                              0, 1,
                              2, 
                              'min-q1 tree', covs)

treeiceblkcvd_2<-pleaserunint('s_iceblk', 'ncvddeath', 
                              'treecatn',
                              1, 0,
                              2, 
                              'q1-q2 tree', covs)

treeiceblkcvd_3<-pleaserunint('s_iceblk', 'ncvddeath', 
                              'treecatn',
                              2, 1,
                              0,  
                              'q2-q3 tree', covs)

tree_iceblk_cvd<-rbind(treeiceblkcvd_1, treeiceblkcvd_2, treeiceblkcvd_3)



PARK_INT<-rbind(park_poverty_all, park_race_all, park_iceblk_all,
                park_poverty_cvd, park_race_cvd, park_iceblk_cvd)
TREE_INT<-rbind(tree_poverty_all, tree_race_all, tree_iceblk_all,
                tree_poverty_cvd, tree_race_cvd, tree_iceblk_cvd)


date<-'2021feb16'
adjust<-'gender_age'

####

interaction<-'park'

write.csv(PARK_INT, paste(date, "Int_", interaction, "_" , adjust, ".csv"))

###
interaction<-'tree'

write.csv(TREE_INT, paste(date, "Int_", interaction, "_" , adjust, ".csv"))

# GRASS CVD

grassracecvd_1<-pleaserunint('s_blk', 'ncvddeath', 
                            'grasscatn',
                            0, 1,
                            2, 
                            'min-q1 grass', covs)

grassracecvd_2<-pleaserunint('s_blk', 'ncvddeath', 
                            'grasscatn',
                            1, 0,
                            2, 
                            'q1-q2 grass', covs)

grassracecvd_3<-pleaserunint('s_blk', 'ncvddeath', 
                            'grasscatn',
                            2, 1,
                            0,  
                            'q2-q3 grass', covs)

grass_race_cvd<-rbind(grassracecvd_1, grassracecvd_2, grassracecvd_3)

#grass poverty cvd cause

grasspovertycvd_1<-pleaserunint('s_pov', 'ncvddeath', 
                               'grasscatn',
                               0, 1,
                               2,  
                               'min-q1 grass', covs)

grasspovertycvd_2<-pleaserunint('s_pov', 'ncvddeath', 
                               'grasscatn',
                               1, 0,
                               2,  
                               'q1-q2 grass', covs)

grasspovertycvd_3<-pleaserunint('s_pov', 'ncvddeath', 
                               'grasscatn',
                               2, 1,
                               0,  
                               'q2-q3 grass', covs)


grass_poverty_cvd<-rbind(grasspovertycvd_1, grasspovertycvd_2, grasspovertycvd_3)

### GRASS ICE CVD


grassiceblkcvd_1<-pleaserunint('s_iceblk', 'ncvddeath', 
                                'grasscatn',
                                0, 1,
                                2,  
                                'min-q1 grass', covs)

grassiceblkcvd_2<-pleaserunint('s_iceblk', 'ncvddeath', 
                                'grasscatn',
                                1, 0,
                                2,  
                                'q1-q2 grass', covs)

grassiceblkcvd_3<-pleaserunint('s_iceblk', 'ncvddeath', 
                                'grasscatn',
                                2, 1,
                                0,  
                                'q2-q3 grass', covs)


grass_iceblk_cvd<-rbind(grassiceblkcvd_1, grassiceblkcvd_2, grassiceblkcvd_3)


###########

grassraceall_1<-pleaserunint('s_blk', 'ndeath', 
                            'grasscatn',
                            0, 1,
                            2, 
                            'min-q1 grass', covs)

grassraceall_2<-pleaserunint('s_blk', 'ndeath', 
                            'grasscatn',
                            1, 0,
                            2, 
                            'q1-q2 grass', covs)

grassraceall_3<-pleaserunint('s_blk', 'ndeath', 
                            'grasscatn',
                            2, 1,
                            0, 
                            'q2-q3 grass', covs)


grass_race_all<-rbind(grassraceall_1, grassraceall_2, grassraceall_3)

#grass poverty all cause

grasspovertyall_1<-pleaserunint('s_pov', 'ndeath', 
                               'grasscatn',
                               0, 1,
                               2,  
                               'min-q1 grass', covs)

grasspovertyall_2<-pleaserunint('s_pov', 'ndeath', 
                               'grasscatn',
                               1, 0,
                               2,  
                               'q1-q2 grass', covs)

grasspovertyall_3<-pleaserunint('s_pov', 'ndeath', 
                               'grasscatn',
                               2, 1,
                               0,  
                               'q2-q3 grass', covs)

grass_poverty_all<-rbind(grasspovertyall_1, grasspovertyall_2, grasspovertyall_3)

# grass ICE all


grassiceblkall_1<-pleaserunint('s_iceblk', 'ndeath', 
                               'grasscatn',
                               0, 1,
                               2,  
                               'min-q1 grass', covs)

grassiceblkall_2<-pleaserunint('s_iceblk', 'ndeath', 
                               'grasscatn',
                               1, 0,
                               2,  
                               'q1-q2 grass', covs)

grassiceblkall_3<-pleaserunint('s_iceblk', 'ndeath', 
                               'grasscatn',
                               2, 1,
                               0,  
                               'q2-q3 grass', covs)


grass_iceblk_all<-rbind(grassiceblkall_1, grassiceblkall_2, grassiceblkall_3)



grass_INT<-rbind(grass_poverty_all, grass_race_all, grass_iceblk_all,
                 grass_poverty_cvd, grass_race_cvd, grass_iceblk_cvd)

##################

adjust<-'gender_age'
####

interaction<-'grass'

write.csv(grass_INT, paste(date, "Int_", interaction, "_" , adjust, ".csv"))

####### NDVI 

ndviracecvd_1<-pleaserunint('s_blk', 'ncvddeath', 
                             'ndvicatn',
                             0, 1,
                             2, 
                             'min-q1 ndvi', covs)

ndviracecvd_2<-pleaserunint('s_blk', 'ncvddeath', 
                             'ndvicatn',
                             1, 0,
                             2, 
                             'q1-q2 ndvi', covs)

ndviracecvd_3<-pleaserunint('s_blk', 'ncvddeath', 
                             'ndvicatn',
                             2, 1,
                             0,  
                             'q2-q3 ndvi', covs)

ndvi_race_cvd<-rbind(ndviracecvd_1, ndviracecvd_2, ndviracecvd_3)

#ndvi poverty cvd cause

ndvipovertycvd_1<-pleaserunint('s_pov', 'ncvddeath', 
                                'ndvicatn',
                                0, 1,
                                2,  
                                'min-q1 ndvi', covs)

ndvipovertycvd_2<-pleaserunint('s_pov', 'ncvddeath', 
                                'ndvicatn',
                                1, 0,
                                2,  
                                'q1-q2 ndvi', covs)

ndvipovertycvd_3<-pleaserunint('s_pov', 'ncvddeath', 
                                'ndvicatn',
                                2, 1,
                                0,  
                                'q2-q3 ndvi', covs)


ndvi_poverty_cvd<-rbind(ndvipovertycvd_1, ndvipovertycvd_2, ndvipovertycvd_3)

### ndvi ICE CVD


ndviiceblkcvd_1<-pleaserunint('s_iceblk', 'ncvddeath', 
                               'ndvicatn',
                               0, 1,
                               2,  
                               'min-q1 ndvi', covs)

ndviiceblkcvd_2<-pleaserunint('s_iceblk', 'ncvddeath', 
                               'ndvicatn',
                               1, 0,
                               2,  
                               'q1-q2 ndvi', covs)

ndviiceblkcvd_3<-pleaserunint('s_iceblk', 'ncvddeath', 
                               'ndvicatn',
                               2, 1,
                               0,  
                               'q2-q3 ndvi', covs)


ndvi_iceblk_cvd<-rbind(ndviiceblkcvd_1, ndviiceblkcvd_2, ndviiceblkcvd_3)


###########

ndviraceall_1<-pleaserunint('s_blk', 'ndeath', 
                             'ndvicatn',
                             0, 1,
                             2, 
                             'min-q1 ndvi', covs)

ndviraceall_2<-pleaserunint('s_blk', 'ndeath', 
                             'ndvicatn',
                             1, 0,
                             2, 
                             'q1-q2 ndvi', covs)

ndviraceall_3<-pleaserunint('s_blk', 'ndeath', 
                             'ndvicatn',
                             2, 1,
                             0, 
                             'q2-q3 ndvi', covs)


ndvi_race_all<-rbind(ndviraceall_1, ndviraceall_2, ndviraceall_3)

#ndvi poverty all cause

ndvipovertyall_1<-pleaserunint('s_pov', 'ndeath', 
                                'ndvicatn',
                                0, 1,
                                2,  
                                'min-q1 ndvi', covs)

ndvipovertyall_2<-pleaserunint('s_pov', 'ndeath', 
                                'ndvicatn',
                                1, 0,
                                2,  
                                'q1-q2 ndvi', covs)

ndvipovertyall_3<-pleaserunint('s_pov', 'ndeath', 
                                'ndvicatn',
                                2, 1,
                                0,  
                                'q2-q3 ndvi', covs)

ndvi_poverty_all<-rbind(ndvipovertyall_1, ndvipovertyall_2, ndvipovertyall_3)

# ndvi ICE all


ndviiceblkall_1<-pleaserunint('s_iceblk', 'ndeath', 
                               'ndvicatn',
                               0, 1,
                               2,  
                               'min-q1 ndvi', covs)

ndviiceblkall_2<-pleaserunint('s_iceblk', 'ndeath', 
                               'ndvicatn',
                               1, 0,
                               2,  
                               'q1-q2 ndvi', covs)

ndviiceblkall_3<-pleaserunint('s_iceblk', 'ndeath', 
                               'ndvicatn',
                               2, 1,
                               0,  
                               'q2-q3 ndvi', covs)


ndvi_iceblk_all<-rbind(ndviiceblkall_1, ndviiceblkall_2, ndviiceblkall_3)



ndvi_INT<-rbind(ndvi_poverty_all, ndvi_race_all, ndvi_iceblk_all,
                 ndvi_poverty_cvd, ndvi_race_cvd, ndvi_iceblk_cvd)

##################

adjust<-'gender_age'
####

interaction<-'ndvi'

write.csv(ndvi_INT, paste(date, "Int_", interaction, "_" , adjust, ".csv"))



int_for=as.formula(paste("d[[outvar]] ~ d[[expvar]] + as.factor(intvar4)  +
                            d[[expvar]]:as.factor(intvar4) +
                           offset(log(totalpop)) +  ", paste(cov,  collapse = "+")))

model <- glm.nb(ndeath ~ s_pov + as.factor(treecatn) + s_pov:treecatn + 
                  as.factor(gender) + as.factor(newage) + offset(log(totalpop)), data=datan)


#####################################################################################
# PLOTS #


library(tidyverse)
library(readxl)
library(zoo)
library(broom)

names<-list.files()
a<-read.csv(names[1])
b<-read.csv(names[2])
c<-read.csv(names[3])
d<-read.csv(names[4])
all<-rbind(a,b,c,d)

colnames(all)<-c("X", "marg", "outcome", "green", "rii", "rii_lci", "rii_uci", 
                 "deviance.main" , "aic.main",       "deviance.int" ,  "aic.int",  "Modifier.level")

#inverse of the ICe statistic results so they are in the same direction. 
all$rii<-ifelse(all$marg=="s_iceblk", 1/all$rii, all$rii)
all$rii_lci<-ifelse(all$marg=="s_iceblk", 1/all$rii_lci, all$rii_lci)
all$rii_uci<-ifelse(all$marg=="s_iceblk", 1/all$rii_uci, all$rii_uci)


series<-c("lo", "mod", "hi")
all$l<-rep(series, 24)  

all$gs1=factor(all$l, levels=c("lo", "mod", "hi"),
            labels=c("Low", "Moderate", "High"))

all$gs2<-ifelse(all$green=="grasscatn", "Proportion grass/shrub",
                ifelse(all$green=="ndvicatn", "NDVI",
                       ifelse(all$green=="parkcatn", "Proportion park access",
                              ifelse(all$green=="treecatn", "Proportion tree",NA))))

all$dem<-ifelse(all$marg=="s_pov", "Proportion living in poverty",
                ifelse(all$marg=="s_blk", "Proportion NH Black", "ICE"))


dta<-all[which(all$outcome=="ndeath"),]

fontsize<-14
ggplot(dta, aes(x=gs1, y=rii)) +
  geom_line(color="gray", aes(group=gs2))+
  geom_hline(yintercept = 1, lty=2)+
  geom_linerange(aes(ymin=rii_lci, ymax=rii_uci), color="gray")+
  geom_point(fill="gray", pch=21, color="black")+
  scale_color_manual(values=c("green", "orange", "red"))+
  scale_fill_manual(values=c("green", "orange", "red"))+
  scale_y_continuous( trans="log", breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4))+
  facet_grid(gs2~dem, labeller = label_wrap_gen(width = 20, multi_line = TRUE))+
  labs(x="Tertile of green space measure", y="Relative Index of Inequality (95% CI)",
       fill="Tertile of green space measure", color="Tertile of green space measure",
       title=" ")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=fontsize, color="black"),
        axis.text=element_text(size=fontsize-2, color="black"),
        axis.title=element_text(face="bold", size=fontsize),
        strip.text =element_text(face="bold", size=fontsize),
        strip.background = element_blank(),
        plot.title=element_text(face="bold", size=fontsize+4, hjust=0.5))
ggsave("all_cause.pdf", width=10, height=9)




dta<-all[which(all$outcome=="ncvddeath"),]

fontsize<-14
ggplot(dta, aes(x=gs1, y=rii)) +
  geom_line(color="gray", aes(group=gs2))+
  geom_hline(yintercept = 1, lty=2)+
  geom_linerange(aes(ymin=rii_lci, ymax=rii_uci), color="gray")+
  geom_point(fill="gray", pch=21, color="black")+
  scale_color_manual(values=c("green", "orange", "red"))+
  scale_fill_manual(values=c("green", "orange", "red"))+
  scale_y_continuous(trans="log", breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4))+
  facet_grid(gs2~dem, labeller = label_wrap_gen(width = 20, multi_line = TRUE))+
  labs(x="Tertile of green space measure", y="Relative Index of Inequality (95% CI)",
       fill="Tertile of green space measure", color="Tertile of green space measure",
       title=" ")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=fontsize, color="black"),
        axis.text=element_text(size=fontsize-2, color="black"),
        axis.title=element_text(face="bold", size=fontsize),
        strip.text =element_text(face="bold", size=fontsize),
        strip.background = element_blank(),
        plot.title=element_text(face="bold", size=fontsize+4, hjust=0.5))
ggsave("cvd.pdf", width=10, height=9)

