
#load("W:/UHC/Schinasi_Infant_Mortality/AHA/equigenic/equigenic_dataprep_workspace.RData")
setwd("W:/UHC/Schinasi_Infant_Mortality/AHA/equigenic")

library(tableone)
load("equigenic_dataprep_new_workspace.RData")
myVars <- c( "hrlctree"   ,       "hrlcgrass_shrub" ,"avg_HAVEPARK_R"  ,
             "avg_FINDFRUT_R"    ,"avg_CURSMK"     ,   "avgpm25"  ,
             "popden_km", "CT10pbacosNPHL16" )


tab3 <- CreateTableOne(vars = myVars, data =env_merged)

prba<-print(tab3)

tab4 <- CreateTableOne(vars = myVars, data = env_merged)

prbb<-print(tab4, nonnormal = myVars)

all<-cbind(prba, prbb)
 
table(env_merged$povcat)


myVars <- c( "hrlctree"   ,       "hrlcgrass_shrub" ,"avg_HAVEPARK_R"  ,"mean_of_max1315",
             "popden_km", "race_blackNH"  ,   
              "pov_allage",  "ICE_blk_inc_1115"  )


tab3 <- CreateTableOne(vars = myVars,strata="povcat", data =env_merged)

prba<-print(tab3, nonnormal=myVars)

write.csv(prba, 'C:/Users/lhs36/Dropbox/Leah Work/AHA WORKING FOLDER/mortality_disparities_equigenic/results/may22023_reviewresponse/descriptive_bypov.csv')


tab3 <- CreateTableOne(vars = myVars,strata="raceblkcat", data =env_merged)

prba<-print(tab3, nonnormal=myVars)

write.csv(prba, 'C:/Users/lhs36/Dropbox/Leah Work/AHA WORKING FOLDER/mortality_disparities_equigenic/results/may22023_reviewresponse/descriptive_byrace.csv')


tab3 <- CreateTableOne(vars = myVars,strata="ice1115blkcat", data =env_merged)

prba<-print(tab3, nonnormal=myVars)

write.csv(prba, 'C:/Users/lhs36/Dropbox/Leah Work/AHA WORKING FOLDER/mortality_disparities_equigenic/results/may22023_reviewresponse/descriptive_byice.csv')



##############################

formatrix<-env_merged[c("hrlctree", "hrlcgrass_shrub", "avg_HAVEPARK_R", 
                        "mean_of_max1315")]

colnames(formatrix)<-c("Trees", "Grass/shrub",  "Parks",
                       "NDVI"
                      )
#"Open space count",
#"CT10pbacosNPHL16",
c<-cor(formatrix)

library(corrplot)
cc<-corrplot(c)
cc<-corrplot(c, method="number", type='lower')
summary(env_merged$pov_allage)

summary(env_merged$ICE_blk_inc_1216
        )