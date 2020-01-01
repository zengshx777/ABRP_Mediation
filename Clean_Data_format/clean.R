##Name list to number
rm(list=ls())
load("ELADSIGC data for Shuxi 9.22.2019.RData")
f = 1;m=1;adv_id=1
age.truncate.female = 18
source("Collapse_Data.R")

source("Clean_Data.R")

name_table=unique(Complete.data$sname)
id=unlist(lapply(name_table,FUN=function(x){which(name_table==x)}))

ID=unlist(lapply(Complete.data$sname,
                 FUN=function(x){which(name_table==x)}))
Complete.data$sname=ID
colnames(Complete.data)[2]="ID"


var_names=colnames(Complete.data)
##Did not remove sex here
##We also used these three variables
##avg_age
##avg_density
del_index=which(var_names%in%c("sid","sex","date","birth","hyb_score",
                               "SCI_F","SCI_M","t","treatment_factor",
                               "adv_01","adv_12","adv_02",
                               "n_adult_mat_kin"))


Complete.data=Complete.data[,-del_index]
Complete.data=Complete.data[,c("ID",
                               "gc",
                               "DSI_F",
                               "DSI_M",
                               "state",
                               "n_adults",
                               "n_adults_squared",
                               "mean_tmax_30d","season",
                               "rain_anom_3mo","proprank",
                               "avg_mat_kin","avg_age","avg_density",
                               "percent_days_with_infant",
                               "percent_days_cycling",
                               "age_sample",
                               "hydroyear",
                               "grp",
                               "adv_rain",
                               "adv_sib",
                               "adv_density",
                               "adv_mom",
                               "adv_mom_rank",
                               "adv_mom_dsi",
                               "adv_cumulative")]


##Output Table
write.csv(cbind(name_table,id),file="sname_to_id.csv")

write.csv(Complete.data,file="Dryad_data.csv")