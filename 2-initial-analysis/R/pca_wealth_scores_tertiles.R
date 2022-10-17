
#-------------------------------------------------------------------------------
# @Organization - Proctor Foundation - UCSF
# @Project - WASHB-SEP-Season
# @Author - Pearl Ante-Testard, pearl.ante@ucsf.edu
# @Description - Creation of the wealth index scores and tertiles
#-------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

library(tidyverse)
library(here)
library(psych)
library(plyr)
library(Rfast)
library(caret)
library(dplyr)
library(survival)
library(furniture)
library(boot)
library(table1)

here::here()

####Read csv
df <- read_csv(file = here::here("manuscripts", "1-data", "0-untouched",
                                "washb-bangladesh-enrol-public.csv"))


scipen=999 # removes scientific notation
#desc_vars <- psych::describe(df)
#desc_vars

#varlist = c("landacre","tubewell","storewat","latown","latslab","latseal","roof","walls","floor",
           # "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
           # "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_clock",
            #"asset_khat","asset_chouki","asset_mobile") #removed "asset_phone" because not enough variation
###removed asset_tvbw and asset_tvcol, because they're redundant with asset_tv and cement which is the same with floor

### removing all WASH-related vars
varlist = c("landacre","roof","walls","floor",
            "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
            "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_clock",
            "asset_khat","asset_chouki","asset_mobile")
varlist<-c("dataid","clusterid","hhid","block", varlist)

varlist_fac = c("roof","walls","floor",
            "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
            "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_clock",
            "asset_khat","asset_chouki","asset_mobile")

stat_vars = c("landacre","walls","floor",
              "elec","asset_refrig","asset_bike","asset_moto",
              "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_mobile","wealthscore","wealth_tertile")


assetPCA<-function(df, varlist, varlist_fac, stat_vars, reorder=F ){

#Subset to only needed variables for subgroup analysis
ret <- df %>%
  subset(select=c(varlist)) ###30 vars including 4 ids

#Select assets
ret<-as.data.frame(ret) 
id<-subset(df, select=c("dataid","clusterid","hhid","block")) ###only ID
ret_assets_comp<-ret[,which(!(colnames(ret) %in% c("dataid","clusterid","hhid","block")))] ###only asset vars
ret_assets_comp[,c(2:17)] <- lapply(ret_assets_comp[,c(2:17)], factor)


#Replace character blank with NA
for(i in 2:ncol(ret_assets_comp)){
ret_assets_comp[,i]<-ifelse(ret_assets_comp[,i]=="",NA,ret_assets_comp[,i])
}

#drop rows with no asset data
id<-id[rowSums(is.na(ret_assets_comp[,5:ncol(ret_assets_comp)])) != ncol(ret_assets_comp)-4,]  
ret_assets_comp<-ret_assets_comp[rowSums(is.na(ret_assets_comp[,5:ncol(ret_assets_comp)])) != ncol(ret_assets_comp)-4,]  


#Drop assets with great missingness
for(i in 1:ncol(ret_assets_comp)){
  cat(colnames(ret_assets_comp)[i],"\n")
  print(table(is.na(ret_assets_comp[,i])))
  print(class((ret_assets_comp[,i])))
}

#### asset_clock has 2859 (vs 2692) NAs, latslab has 238 (vs 5313) NAs, latseal has 880 (vs 4871) NAs

### removing asset_clock
#cols.dont.want <- c("asset_clock","latseal")
cols.dont.want <- c("asset_clock")
ret_assets_comp <- ret_assets_comp[, ! names(ret_assets_comp) %in% cols.dont.want, drop = F]

#create level for missing factor levels
table(is.na(ret_assets_comp))
for(i in 2:ncol(ret_assets_comp)){
  ret_assets_comp[,i]<-as.character(ret_assets_comp[,i])
  ret_assets_comp[is.na(ret_assets_comp[,i]),i]<-"miss"
  ret_assets_comp[,i]<-as.factor(ret_assets_comp[,i])
  
}

ret_assets_comp$landacre[is.na(ret_assets_comp$landacre)] <- mean(ret_assets_comp$landacre, na.rm = T) ###repLace NAs with the mean
table(is.na(ret_assets_comp))

#Convert factors into indicators
ret_assets_comp[,2:length(ret_assets_comp)]<-droplevels(ret_assets_comp[,2:length(ret_assets_comp)])
#ret_assets_compm<-design_matrix(ret_assets_comp[,2:23])

#ret_assets_compm <- data.frame(ret_assets_comp[, ! colnames(ret_assets_comp) %in% c("tubewell","storewat","latown","latslab","roof","walls","floor",
                                                                               #     "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
                                                                                #    "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_khat",
                                                                                  #  "asset_chouki","asset_mobile")],
                               #model.matrix(~tubewell+storewat+latown+latslab+roof+walls+floor+elec+asset_radio+asset_refrig+asset_bike+asset_moto+
                                              #asset_sewmach+asset_tv+asset_wardrobe+asset_table+asset_chair+asset_khat+asset_chouki+asset_mobile, ret_assets_comp))

Formula <- as.character(paste("~", "ret_assets_comp[,",2,"]", sep=''))
for (i in 3:length(varlist_fac)) {
  #if(i==3){Formula<-as.character(paste(Formula,"ret_assets_comp[,",i,"]",sep=""))}
  #if(i!=3){
  Formula<-as.character(paste(Formula,"+","ret_assets_comp[,",i,"]",sep=""))
  #}
}
Formula<-as.formula(Formula)

###removing WASH-related vars
ret_assets_compm <- data.frame(ret_assets_comp[, ! colnames(ret_assets_comp) %in% varlist_fac],
                               model.matrix(Formula, ret_assets_comp))


#Remove columns with almost no variance
if(length(nearZeroVar(ret_assets_compm))>0){
  ret_assets_compm <-ret_assets_compm[,-nearZeroVar(ret_assets_compm)]
} ####asset_radio and "roof" got removed

## Convert the data into matrix ##
ret_assets_compm <-as.matrix(ret_assets_compm)

for(y in 1:ncol(ret_assets_compm)) {
  for(x in 1:nrow(ret_assets_compm)) {
    if (is.na(ret_assets_compm[x,y])) ret_assets_compm[x,y] = 0
  }}

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(ret_assets_compm) 
screeplot(princ.return)

## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1] 

pr.cp <- ret_assets_compm %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
HHwealth <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.

#HHwealth=princ.return$scores[,1] ##factor scores from PC 1

#ret_assets_compm<-as.data.frame(ret_assets_compm)
#ret_assets_compm$HHwealth_nowash<-HHwealth

#Create 3-level household weath index
tertiles<-quantile(HHwealth, probs=seq(0, 1, 1/3))
print(tertiles)
ret_assets_compm<-as.data.frame(ret_assets_compm)
ret_assets_compm$HHwealth<-HHwealth
ret_assets_compm$HHwealth_ter<-rep(1, nrow(ret_assets_compm))
ret_assets_compm$HHwealth_ter[HHwealth>=tertiles[1]]<-1
ret_assets_compm$HHwealth_ter[HHwealth>=tertiles[2]]<-2
ret_assets_compm$HHwealth_ter[HHwealth>=tertiles[3]]<-3
table(ret_assets_compm$HHwealth_ter)

if(reorder==T){
  ret_assets_compm$HHwealth_ter<-factor(ret_assets_compm$HHwealth_ter, levels=c("1", "2","3"))
}else{
levels(ret_assets_compm$HHwealth_ter)<-c("1", "2","3")
}

#Table assets by pca quintile to identify wealth/poverty levels
d<-data.frame(id, ret_assets_compm)
#stat_vars = c("landacre","tubewell","storewat","latown","latslab","walls","floor",
             # "elec","asset_refrig","asset_bike","asset_moto",
             # "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_mobile","wealthscore_wash","wealthquin")

###removing WASH-related vars
#stat_vars = c("landacre","walls","floor",
              #"elec","asset_refrig","asset_bike","asset_moto",
              #"asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_mobile","wealth_tertile","wealthscore")

colnames(d) <- c("dataid","clusterid","hhid","block", stat_vars)

#Save just the wealth data
pca.wealth<-d %>% subset(select=c(dataid,clusterid,hhid,block,wealthscore,wealth_tertile))
pca.wealth$dataid<-as.numeric(as.character(pca.wealth$dataid))

d <-df %>% subset(., select=c("dataid","clusterid","hhid","block"))
d$dataid<-as.numeric(as.character(d$dataid))
d<-left_join(d, pca.wealth, by=c("dataid","clusterid","hhid","block"))

return(as.data.frame(d))

}

d <- assetPCA(df, varlist = varlist, varlist_fac = varlist_fac, stat_vars = stat_vars, reorder = F)

saveRDS(d, here::here("manuscripts", "1-data", "1-temp",
                      "pca_tertiles_scores_nowashchar.rds"))


