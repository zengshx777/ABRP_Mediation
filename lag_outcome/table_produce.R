rm(list=ls())
##Grids for the orthogonality condition
Grid.num=40
work_grid=seq(0,1,length=Grid.num)
age_grids=work_grid*(18-4+0.0002)+4-0.0001
f=1
for (m in c(1:5)){
  table_output=NULL
  word_table=NULL
  setwd("~/Downloads/third year/FPCA_1007")
source("varcodebook_0923.R")

for (adv_id in c(1:6,8,9)){
setwd("~/Downloads/third year/FPCA_1007/WithoutHyb_New/")
#setwd("~/Downloads/third year/FPCA_1007/New_Cumu_1102/")
#setwd("~/Downloads/third year/FPCA_1007/WithoutHyb_New/")
load(file=paste(mediation.index[m],adv_id,f,"Collect_withHybScore.RData",sep="_"));abnormal=0
#load(file=paste(mediation.index[m],adv_id,f,"Collect_withHybScore.RData",sep="_"));abnormal=1
#Adjust Value
#target_values=matrix(0,8,3)
#adjusting
setwd("~/Downloads/third year/FPCA_1007")
source("table_loading.R")
setwd("~/Downloads/third year/FPCA_1007/WithoutHyb_New/Results_data")
save(
  total_effect_mean,
  total_effect_down,
  total_effect_up,
  mediator_effect_mean,
  mediator_effect_down,
  mediator_effect_up,
  gamma,
  direct_effect_mean,
  direct_effect_down,
  direct_effect_up,
  indirect_process_mean,
  indirect_process_down,
  indirect_process_up,
  file = paste(mediation.index[m], adv_id, f, "Collect_Result.RData",sep="_")
)
#setwd("~/Downloads/third year/FPCA_1007/New_Cumu_1102")
#setwd("~/Downloads/third year/FPCA_1007/WithoutHyb_New/png_plots")
setwd("~/Downloads/third year/FPCA_1007/WithoutHyb_New/pdf_plots")
# png(file=paste(adverse.index[adv_id],mediation.index[m],f,"Decom.png",sep="_"),
#     width=400,height=280)
pdf(file=paste(adverse.index[adv_id],mediation.index[m],f,"Decom.pdf",sep="_"),
    width=8,height=5)
plot(age_grids,total_effect_mean,ylim=range(total_effect_up+0.1,total_effect_down-0.1),type='l',col="blue",
     ylab="Percent change on fGC",xlab="Age",yaxt="n")
axis(2, at=pretty(range(total_effect_up+0.1,total_effect_down-0.1)), lab=pretty(range(total_effect_up+0.1,total_effect_down-0.1))*100,las=T)
lines(age_grids,total_effect_down,lty=2)
lines(age_grids,total_effect_up,lty=2)
abline(h=0)



lines(age_grids,indirect_process_mean,col="red")
lines(age_grids,indirect_process_up,lty=2)
lines(age_grids,indirect_process_down,lty=2)
abline(h=0)

legend("topright",legend=c("Total effect","Mediation effect"),col=c("blue","red"),
       lty=1)
dev.off()
aggregate= c(mean(total_effect_mean),mean(total_effect_down),
             mean(total_effect_up),
             mean(mediator_effect_mean),mean(mediator_effect_down),
             mean(mediator_effect_up),
             mean(gamma),quantile(gamma,0.025),
             quantile(gamma,0.975),
             mean(direct_effect_mean),mean(direct_effect_down),
             mean(direct_effect_up),
             mean(indirect_process_mean),mean(indirect_process_down),
             mean(indirect_process_up))

table_output=rbind(table_output,aggregate)

aggre_mean=c(mean(total_effect_mean),mean(mediator_effect_mean),
       mean(gamma),mean(direct_effect_mean),mean(indirect_process_mean))

aggre_ci=c(paste("(",format(mean(total_effect_down),digits=3),",",format(mean(total_effect_up),digits=3),")",sep=""),
  paste("(",format(mean(mediator_effect_down),digits=3),",",format(mean(mediator_effect_up),digits=3),")",sep=""),
  paste("(",format(quantile(gamma,0.025),digits=3),",",format(quantile(gamma,0.975),digits=3),")",sep=""),
  paste("(",format(mean(direct_effect_down),digits=3),",",format(mean(direct_effect_up),digits=3),")",sep=""),
  paste("(",format(mean(indirect_process_down),digits=3),",",format(mean(indirect_process_up),digits=3),")",sep=""))
  
word_table=rbind(word_table,aggre_mean,aggre_ci)

}
#row.names(table_output)=c("0 vs >=2")
# row.names(table_output)=c("0 vs >=1","1 vs >=2")
# colnames(table_output)=c("total","total_down","total_up",
#                          "effect_m","effect_m_down","effect_m_up",
#                          "miti","miti_down","miti_up",
#                          "direct","direct_down","direct_up",
#                          "indirect","indirect_down","indirect_up")
# 
# #write.csv(table_output,file=paste(mediation.index[m],"summary_0v2.csv",sep="_"))
# #row.names(word_table)=rep("0 vs >=2",each=2)
# row.names(word_table)=rep(c("0 vs >=1","1 vs >=2"),each=2)
# colnames(word_table)=c("total","effect_m","miti","direct","indirect")
#write.csv(word_table,file=paste(mediation.index[m],"summary_for_word_0v2.csv",sep="_"))
}
  