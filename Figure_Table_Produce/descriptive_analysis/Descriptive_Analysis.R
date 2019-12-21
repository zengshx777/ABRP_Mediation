###Descriptive Analysis Part
setwd("~/Dropbox/Things we generated during Shuxi's visit/R code/descriptive_analysis")
rm(list=ls())
# setwd("C:/Users/Shuxi ZENG/Dropbox/Third Year/FPCA_New/FPCA_1005")
load("ELADSIGC data for Shuxi 9.22.2019.RData")

f = 1
age.truncate.female = 18
source("Collapse_Data.R")

covariate_descriptive = c(
  "age_sample",
  "n_adults",
  "avg_mat_kin",
  "percent_days_cycling",
  "percent_days_with_infant",
  "mean_tmax_30d",
  "rain_anom_3mo",
  "proprank",
  "hyb_score"
)
table_num = NULL
n_track=NULL
time_span=NULL



#Number of individuals
for (m in 1:5)
{
  for (adv_id in (1:9))
  {
    source("Clean_Data_0923.R")
    
    
    table_num = rbind(table_num, c(
      m,
      adv_id,
      length(unique(Complete.data$sname)),
      length(unique(Complete.data$grp)),
      length(Complete.data$sname)
    ))
    n_track = rbind(n_track,
                    c(m,adv_id,nsample_track,nunit_track))
    individual_time_span = aggregate(
      Complete.data$age_sample,
      by = list(Complete.data$sname),
      FUN = function(x) {
        (max(x) - min(x)) * 12
      }
    )$x
    time_span= rbind(time_span,
      c(mean(individual_time_span),
                     median(individual_time_span),
                     sd(individual_time_span),
                     range(individual_time_span)))
    
    
  }
}
colnames(table_num) = c(
  "Mediator Index",
  "Adversity Index",
  "Number of Individuals",
  "Number of Social Groups",
  "Sample Size"
)
colnames(n_track)=c(
  "Mediator Index",
  "Adversity Index",rep(
    c("BeforeClean","MissingFilter",
      "AgeTruncate","SparseDelete"),
    2
  ))
colnames(time_span)=c(
  "Mean","Median","SD","min","max"
)
write.csv(table_num, file = "../../Tables/Basic descriptive statistics/number_summary.csv")
write.csv(n_track,file="../../Tables/Basic descriptive statistics/sample_track_summary.csv")
write.csv(time_span,file="../../Tables/Basic descriptive statistics/time_span_summary.csv")

f=1;m=1;adv_id=1
source("Clean_Data_0923.R")
summary_cov=t(apply(
  Complete.data[, covariate_descriptive],
  2,
  FUN = function(x) {
    c(mean(x[!is.na(x)]), median(x[!is.na(x)]), sd(x[!is.na(x)]),range(x[!is.na(x)]))
  }
))
colnames(summary_cov)=c("mean","median","sd","min","max")
write.csv(summary_cov,file="../../Tables/Basic descriptive statistics/covariate_descriptive.csv")

write.csv(c(table(Complete.data$state),
table(Complete.data$hydroyear),
table(Complete.data$season)),file="../../Tables/Basic descriptive statistics/categorical_variable_descriptive.csv")


##Adversity Counts Histogram
png("../../Figures/Descriptive_Summary/Adversity_Sample_Count.png")
hist(
  gender.data$adv_cumulative,
  breaks = rep(0:5, each = 2) + c(-0.4, 0.4),
  main = "Histogram of Cumulative Adversity Counts",
  freq = TRUE,
  xlab = "Cumulative Adversities",
  ylab = "Sample Frequency"
)
dev.off()

png("../../Figures/Descriptive_Summary/Cumulative_Counts.png")
hist(aggregate(Complete.data$adv_cumulative,by=list(Complete.data$sname),mean)$x,
     breaks = rep(0:5, each = 2) + c(-0.4, 0.4),
     main = "Histogram of Cumulative Adversity Counts",
     freq = TRUE,
     xlab = "Cumulative Adversities",
     ylab = "Count of Females")
dev.off()

write.csv(t(unlist(apply(
  Complete.data[, adverse.index[-7]],
  2,
  FUN = function(x) {
    table(x)
  }
))),file="../../Tables/Basic descriptive statistics/adv_sample_counts.csv")

individual_accum=aggregate(Complete.data$adv_cumulative,by=list(Complete.data$sname),mean)$x
adv_individual_summary=c(mean(individual_accum),median(individual_accum),
  sd(individual_accum),range(individual_accum))
names(adv_individual_summary)=c("mean","median","sd","min","max")
write.csv(adv_individual_summary,file="../../Tables/Basic descriptive statistics/accumulative_individual_summary.csv")

write.csv(t(unlist(apply(
  aggregate(
    Complete.data[, adverse.index[-7]],
    by = list(Complete.data$sname),
    FUN = function(x) {
      mean(x)
    }
  )[, -1], 2, table
))),file="../../Tables/Basic descriptive statistics/adv_individual_counts.csv")
#Number of GC samples
gc_obs_counts=aggregate(Complete.data$gc,by=list(Complete.data$sname),length)$x

png("../../Figures/Descriptive_Summary/GC_obs_individual_counts.png")
hist(gc_obs_counts,
     breaks = rep(c(0,50,100,150,200,250,300), each = 2)+c(-25,25),
     freq=TRUE,
     xlab="Number of GC Observations",
     ylab="Sample Counts",
     main="Histogram of GC Observations")
dev.off()

gc_counts=c(mean(gc_obs_counts),median(gc_obs_counts),sd(gc_obs_counts),range(gc_obs_counts))
names(gc_counts)=c("mean","median","sd","min","max")
write.csv(gc_counts,file="../../Tables/Basic descriptive statistics/gc_sample_counts.csv")


###Mediator Description
mediator_counts_table=NULL
mediator_stat_table=NULL
outcome_stat_table=NULL
adv_id=1
for (m in 1:5)
{
  source("Clean_Data_0923.R")
  unique_counts=aggregate(Complete.data$hydroyear,by=list(Complete.data$sname),FUN=function(x){length(unique(x))})$x
  mediator_counts_table=rbind(mediator_counts_table,
        c(mean(unique_counts),median(unique_counts),sd(unique_counts),range(unique_counts),sum(unique_counts) ))
  mediator_stat_table=rbind(mediator_stat_table,c(mean(Complete.data[,mediation.index[m]]),median(Complete.data[,mediation.index[m]]),
  sd(Complete.data[,mediation.index[m]]),range(Complete.data[,mediation.index[m]])))
  outcome_stat_table=rbind(outcome_stat_table,c(mean(Complete.data[,"gc"]),median(Complete.data[,"gc"]),
                                                sd(Complete.data[,"gc"]),range(Complete.data[,"gc"])),
                           c(mean(exp(Complete.data[,"gc"])),median(exp(Complete.data[,"gc"])),
                             sd(exp(Complete.data[,"gc"])),range(exp(Complete.data[,"gc"]))))
}
row.names(mediator_counts_table)=mediation.index
colnames(mediator_counts_table)=c("mean","median","sd","min","max","total")
write.csv(mediator_counts_table,file="../../Tables/Basic descriptive statistics/mediator_obs_counts.csv")

row.names(mediator_stat_table)=mediation.index
colnames(mediator_stat_table)=c("mean","median","sd","min","max")
row.names(outcome_stat_table)=rep(c("log gc","gc"),5)
colnames(outcome_stat_table)=c("mean","median","sd","min","max")
write.csv(mediator_stat_table,file="../../Tables/Basic descriptive statistics/mediator_stat.csv")
write.csv(outcome_stat_table,file="../../Tables/Basic descriptive statistics/outcome_stat.csv")

