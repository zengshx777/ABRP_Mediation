setwd("~/Dropbox/Things we generated during Shuxi's visit/R code/Eigen_function_Ordination_draw")
library(ggplot2)
library(gridExtra)
library(plotrix)
library(dplyr)
rm(list=ls())
load("plot_DSI_F_pc.RData")
snames_cumadversities <- distinct(Complete.data, sname, adv_cumulative)
zero_adv_name_list=snames_cumadversities$sname[snames_cumadversities$adv_cumulative==0]
one_adv_name_list=snames_cumadversities$sname[snames_cumadversities$adv_cumulative==1]
two_adv_name_list=snames_cumadversities$sname[snames_cumadversities$adv_cumulative>1]

#set.seed(1029)
# zero_name=sample(zero_adv_name_list,1)
# one_name=sample(one_adv_name_list,1)
# two_name=sample(two_adv_name_list,1)
# adv_name_list=c(zero_name,one_name,two_name)
zero_name="EAG"
one_name="OCT"
two_name="GUI"
adv_name_list=c("EAG","OCT","GUI")

# zero_name="VIG"
# one_name="ELA"
# two_name="LOX"
# adv_name_list=c("VIG","ELA","LOX")
#########################################################
pca_scores_gcs <- data.frame(score_1, score_2, Treatment_Ind,sname=unique(Complete.data$sname))
pca_scores_gcs_cumadversities <- data.frame(pca_scores_gcs, snames_cumadversities)
pca_scores_gcs_cumadversities$adv_cum_asfactor <- as.factor(pca_scores_gcs_cumadversities$adv_cumulative)
levels(pca_scores_gcs_cumadversities$adv_cum_asfactor) <- list(zero=(0), one=(1), twoplus=c(2,3,4,5))


Residuals=unlist(residuals)
process_mean=process_value_fitted
Complete.data$resids <-Residuals
Complete.data$resids <- as.numeric(Complete.data$resids)

#Need to save process data into new data frame with sname in it

Process.data_gcs=data.frame(
  sname=rep(name.list,each=length(age_grids)),
  grids=rep(age_grids,length(name.list)),
  process.val=as.vector(process_mean))

three_residuals <- subset(Complete.data, sname %in% adv_name_list)
three_residuals$adv_num="0"
three_residuals$adv_num[three_residuals$sname==one_name]="1"
three_residuals$adv_num[three_residuals$sname==two_name]="2+"
three_residuals$adv_num=as.factor(three_residuals$adv_num)

three_process <- subset(Process.data_gcs, sname %in% adv_name_list)
three_process$adv_num="0"
three_process$adv_num[three_process$sname==one_name]="1"
three_process$adv_num[three_process$sname==two_name]="2+"
three_process$adv_num=as.factor(three_process$adv_num)

max_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),max)
min_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),min)

#Truncation
three_process = rbind(subset(
  three_process,(sname == max_time$Group.1[1] &three_process$grids >=min_time$x[1]-0.2&
     three_process$grids <= max_time$x[1]+0.2)),
subset(three_process,
    (sname == max_time$Group.1[2] &three_process$grids >=min_time$x[2]-0.2&
       three_process$grids <= max_time$x[2]+0.2)),
subset(three_process,
    (sname == max_time$Group.1[3] &three_process$grids >=min_time$x[3]-0.2&
    three_process$grids <= max_time$x[3]+0.2)
))

eigen_data=data.frame(age_grids=rep(age_grids,2),
                      eigen=c(eigen_1,eigen_2),
                      eigen_down=c(eigen_1_down,eigen_2_down),
                      eigen_up=c(eigen_1_up,eigen_2_up),
                      id=as.factor(rep(c(1,2),each=length(age_grids))))
p_eigen=ggplot()+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen,col=id))+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_up,col=id),lty=2)+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_down,col=id),lty=2)+
  scale_color_brewer(palette="Set1", labels=paste0(c("1st  PC:","2nd PC:"),format(100*var_explain,digits=4),"%"))+
  labs(x = "Age at sample collection",
       y = "Eigenfunction",
       color="")+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_traj=ggplot() +
  geom_line(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
#  geom_point(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  geom_line(linetype = 2, lwd=0.8,data=three_process,aes(x=grids, y=process.val,color=factor(adv_num)))+
  scale_color_brewer(palette="Dark2", labels=paste0(adv_name_list,c(":0",":1",":2+")))+
  labs(x = "Age at sample collection",
       y = "Social integration residuals",
       color="Individual name")+ylim(min(three_residuals$resids)-0.1,max(three_residuals$resids)+0.1)+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_ordination=ggplot(pca_scores_gcs_cumadversities, aes(score_1, score_2, color=adv_cum_asfactor))+
  geom_point(size=2.0)+
  stat_ellipse(size=1.0)+
  scale_color_brewer(palette="Dark2", labels=c("0", "1", "2+"))+
  # geom_label(data=data.frame(name=c("Q1","Q2","Q3","Q4"),
  #                          x=c(min(score_1)+1,max(score_1)-1,max(score_1)-1,min(score_1)+1),
  #                          y=c(2,2,-2,-2)),aes(label=name,x=x,y=y))+
  geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%adv_name_list),
             aes(label=sname),show.legend= F,hjust=-0.5,vjust=0.5)+
  # geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%name_combined),
  #            aes(label=sname))+
  geom_vline(xintercept = median(score_1))+
  geom_hline(yintercept = median(score_2))+
  xlim(-0.4,0.4)+
  ylim(-0.6,0.6)+
  # ylim(quantile(pca_scores_gcs_cumadversities$score_2,0.01),quantile(pca_scores_gcs_cumadversities$score_2,0.99))+
  # xlim(quantile(pca_scores_gcs_cumadversities$score_1,0.01),quantile(pca_scores_gcs_cumadversities$score_1,0.99))+
  labs(x="Score on PC 1", y="Score on PC 2", color="Number of adversities")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.02, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
####################################################
pdf("../../Figures/eigen_function/DSI_F_eigen.pdf")
p_eigen
dev.off()

pdf("../../Figures/Ordination/DSI_F_ordination.pdf")
p_ordination
dev.off()

pdf("../../Figures/Individual_Trajectory_Subgroup_Comparison/DSI_F_trajectory.pdf")
p_traj
dev.off()

pdf("../../Figures/Collect_Figures_Eigen_ordination_individual_together/Collected_DSI_F.pdf",height=14,width=8)
grid.arrange(p_eigen,p_ordination,p_traj,nrow=3)
dev.off()


load("plot_gc_pc.RData")
##############################################
pca_scores_gcs <- data.frame(score_1, score_2, Treatment_Ind,sname=unique(Complete.data$sname))
pca_scores_gcs_cumadversities <- data.frame(pca_scores_gcs, snames_cumadversities)
pca_scores_gcs_cumadversities$adv_cum_asfactor <- as.factor(pca_scores_gcs_cumadversities$adv_cumulative)
levels(pca_scores_gcs_cumadversities$adv_cum_asfactor) <- list(zero=(0), one=(1), twoplus=c(2,3,4,5))


Residuals=unlist(residuals)
process_mean=process_value_fitted
Complete.data$resids <-Residuals
Complete.data$resids <- as.numeric(Complete.data$resids)

#Need to save process data into new data frame with sname in it

Process.data_gcs=data.frame(
  sname=rep(name.list,each=length(age_grids)),
  grids=rep(age_grids,length(name.list)),
  process.val=as.vector(process_mean))

three_residuals <- subset(Complete.data, sname %in% adv_name_list)
three_residuals$adv_num="0"
three_residuals$adv_num[three_residuals$sname==one_name]="1"
three_residuals$adv_num[three_residuals$sname==two_name]="2+"
three_residuals$adv_num=as.factor(three_residuals$adv_num)

three_process <- subset(Process.data_gcs, sname %in% adv_name_list)
three_process$adv_num="0"
three_process$adv_num[three_process$sname==one_name]="1"
three_process$adv_num[three_process$sname==two_name]="2+"
three_process$adv_num=as.factor(three_process$adv_num)

max_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),max)
min_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),min)

#Truncation
three_process = rbind(subset(
  three_process,(sname == max_time$Group.1[1] &three_process$grids >=min_time$x[1]-0.2&
                   three_process$grids <= max_time$x[1]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[2] &three_process$grids >=min_time$x[2]-0.2&
            three_process$grids <= max_time$x[2]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[3] &three_process$grids >=min_time$x[3]-0.2&
            three_process$grids <= max_time$x[3]+0.2)
  ))

eigen_data=data.frame(age_grids=rep(age_grids,2),
                      eigen=c(eigen_1,eigen_2),
                      eigen_down=c(eigen_1_down,eigen_2_down),
                      eigen_up=c(eigen_1_up,eigen_2_up),
                      id=as.factor(rep(c(1,2),each=length(age_grids))))


p_eigen=ggplot()+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen,col=id))+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_up,col=id),lty=2)+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_down,col=id),lty=2)+
  scale_color_brewer(palette="Set1", labels=paste0(c("1st  PC:","2nd PC:"),format(100*var_explain,digits=4),"%"))+
  labs(x = "Age at sample collection",
       y = "Eigenfunction",
       color="")+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_traj=ggplot() +
  geom_line(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  #  geom_point(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  geom_line(linetype = 2, lwd=0.8,data=three_process,aes(x=grids, y=process.val,color=factor(adv_num)))+
  scale_color_brewer(palette="Dark2", labels=paste0(adv_name_list,c(":0",":1",":2+")))+
  labs(x = "Age at sample collection",
       y = "fGC residuals",
       color="Individual name")+ylim(min(three_residuals$resids)-0.1,max(three_residuals$resids)+0.1)+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
#p_traj



p_ordination=ggplot(pca_scores_gcs_cumadversities, aes(score_1, score_2, color=adv_cum_asfactor))+
  geom_point(size=2.0)+
  stat_ellipse(size=1.0)+
  scale_color_brewer(palette="Dark2", labels=c("0", "1", "2+"))+
  # geom_label(data=data.frame(name=c("Q1","Q2","Q3","Q4"),
  #                          x=c(min(score_1)+1,max(score_1)-1,max(score_1)-1,min(score_1)+1),
  #                          y=c(2,2,-2,-2)),aes(label=name,x=x,y=y))+
  geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%adv_name_list),
             aes(label=sname),show.legend= F,hjust=-0.5,vjust=0.5)+
  # geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%name_combined),
  #            aes(label=sname))+
  geom_vline(xintercept = median(score_1))+
  geom_hline(yintercept = median(score_2))+
  # ylim(quantile(pca_scores_gcs_cumadversities$score_2,0.01),quantile(pca_scores_gcs_cumadversities$score_2,0.99))+
  # xlim(quantile(pca_scores_gcs_cumadversities$score_1,0.01),quantile(pca_scores_gcs_cumadversities$score_1,0.99))+
  labs(x="Score on PC 1", y="Score on PC 2", color="Number of adversities")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.02, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
###############################################
pdf("../../Figures/eigen_function/gc_eigen.pdf")
p_eigen
dev.off()

pdf("../../Figures/Ordination/gc_ordination.pdf")
p_ordination
dev.off()

pdf("../../Figures/Individual_Trajectory_Subgroup_Comparison/gc_trajectory.pdf")
p_traj
dev.off()

pdf("../../Figures/Collect_Figures_Eigen_ordination_individual_together/Collected_gc.pdf",height=14,width=8)
grid.arrange(p_eigen,p_ordination,p_traj,nrow=3)
dev.off()




##################################
load("plot_DSI_M_pc.RData")
#########################################################
snames_cumadversities <- distinct(Complete.data, sname, adv_cumulative)
pca_scores_gcs <- data.frame(score_1, score_2, Treatment_Ind,sname=unique(Complete.data$sname))
pca_scores_gcs_cumadversities <- data.frame(pca_scores_gcs, snames_cumadversities)
pca_scores_gcs_cumadversities$adv_cum_asfactor <- as.factor(pca_scores_gcs_cumadversities$adv_cumulative)
levels(pca_scores_gcs_cumadversities$adv_cum_asfactor) <- list(zero=(0), one=(1), twoplus=c(2,3,4,5))


Residuals=unlist(residuals)
process_mean=process_value_fitted
Complete.data$resids <-Residuals
Complete.data$resids <- as.numeric(Complete.data$resids)

#Need to save process data into new data frame with sname in it

Process.data_gcs=data.frame(
  sname=rep(name.list,each=length(age_grids)),
  grids=rep(age_grids,length(name.list)),
  process.val=as.vector(process_mean))

three_residuals <- subset(Complete.data, sname %in% adv_name_list)
three_residuals$adv_num="0"
three_residuals$adv_num[three_residuals$sname==one_name]="1"
three_residuals$adv_num[three_residuals$sname==two_name]="2+"
three_residuals$adv_num=as.factor(three_residuals$adv_num)

three_process <- subset(Process.data_gcs, sname %in% adv_name_list)
three_process$adv_num="0"
three_process$adv_num[three_process$sname==one_name]="1"
three_process$adv_num[three_process$sname==two_name]="2+"
three_process$adv_num=as.factor(three_process$adv_num)

max_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),max)
min_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),min)

#Truncation
three_process = rbind(subset(
  three_process,(sname == max_time$Group.1[1] &three_process$grids >=min_time$x[1]-0.2&
                   three_process$grids <= max_time$x[1]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[2] &three_process$grids >=min_time$x[2]-0.2&
            three_process$grids <= max_time$x[2]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[3] &three_process$grids >=min_time$x[3]-0.2&
            three_process$grids <= max_time$x[3]+0.2)
  ))

eigen_data=data.frame(age_grids=rep(age_grids,2),
                      eigen=c(eigen_1,eigen_2),
                      eigen_down=c(eigen_1_down,eigen_2_down),
                      eigen_up=c(eigen_1_up,eigen_2_up),
                      id=as.factor(rep(c(1,2),each=length(age_grids))))
p_eigen=ggplot()+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen,col=id))+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_up,col=id),lty=2)+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_down,col=id),lty=2)+
  scale_color_brewer(palette="Set1", labels=paste0(c("1st  PC:","2nd PC:"),format(100*var_explain,digits=4),"%"))+
  labs(x = "Age at sample collection",
       y = "Eigenfunction",
       color="")+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_traj=ggplot() +
  geom_line(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  #  geom_point(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  geom_line(linetype = 2, lwd=0.8,data=three_process,aes(x=grids, y=process.val,color=factor(adv_num)))+
  scale_color_brewer(palette="Dark2", labels=paste0(adv_name_list,c(":0",":1",":2+")))+
  labs(x = "Age at sample collection",
       y = "Social integration residuals",
       color="Individual name")+ylim(min(three_residuals$resids)-0.1,max(three_residuals$resids)+0.1)+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_ordination=ggplot(pca_scores_gcs_cumadversities, aes(score_1, score_2, color=adv_cum_asfactor))+
  geom_point(size=2.0)+
  stat_ellipse(size=1.0)+
  scale_color_brewer(palette="Dark2", labels=c("0", "1", "2+"))+
  # geom_label(data=data.frame(name=c("Q1","Q2","Q3","Q4"),
  #                          x=c(min(score_1)+1,max(score_1)-1,max(score_1)-1,min(score_1)+1),
  #                          y=c(2,2,-2,-2)),aes(label=name,x=x,y=y))+
  geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%adv_name_list),
             aes(label=sname),show.legend= F,hjust=-0.5,vjust=0.5)+
  # geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%name_combined),
  #            aes(label=sname))+
  geom_vline(xintercept = median(score_1))+
  geom_hline(yintercept = median(score_2))+
  # ylim(quantile(pca_scores_gcs_cumadversities$score_2,0.01),quantile(pca_scores_gcs_cumadversities$score_2,0.99))+
  # xlim(quantile(pca_scores_gcs_cumadversities$score_1,0.01),quantile(pca_scores_gcs_cumadversities$score_1,0.99))+
  labs(x="Score on PC 1", y="Score on PC 2", color="Number of adversities")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.02, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
####################################################
pdf("../../Figures/eigen_function/DSI_M_eigen.pdf")
p_eigen
dev.off()

pdf("../../Figures/Ordination/DSI_M_ordination.pdf")
p_ordination
dev.off()

pdf("../../Figures/Individual_Trajectory_Subgroup_Comparison/DSI_M_trajectory.pdf")
p_traj
dev.off()

pdf("../../Figures/Collect_Figures_Eigen_ordination_individual_together/Collected_DSI_M.pdf",height=14,width=8)
grid.arrange(p_eigen,p_ordination,p_traj,nrow=3)
dev.off()


load("plot_SCI_F_pc.RData")
#########################################################
snames_cumadversities <- distinct(Complete.data, sname, adv_cumulative)
pca_scores_gcs <- data.frame(score_1, score_2, Treatment_Ind,sname=unique(Complete.data$sname))
pca_scores_gcs_cumadversities <- data.frame(pca_scores_gcs, snames_cumadversities)
pca_scores_gcs_cumadversities$adv_cum_asfactor <- as.factor(pca_scores_gcs_cumadversities$adv_cumulative)
levels(pca_scores_gcs_cumadversities$adv_cum_asfactor) <- list(zero=(0), one=(1), twoplus=c(2,3,4,5))


Residuals=unlist(residuals)
process_mean=process_value_fitted
Complete.data$resids <-Residuals
Complete.data$resids <- as.numeric(Complete.data$resids)

#Need to save process data into new data frame with sname in it

Process.data_gcs=data.frame(
  sname=rep(name.list,each=length(age_grids)),
  grids=rep(age_grids,length(name.list)),
  process.val=as.vector(process_mean))

three_residuals <- subset(Complete.data, sname %in% adv_name_list)
three_residuals$adv_num="0"
three_residuals$adv_num[three_residuals$sname==one_name]="1"
three_residuals$adv_num[three_residuals$sname==two_name]="2+"
three_residuals$adv_num=as.factor(three_residuals$adv_num)

three_process <- subset(Process.data_gcs, sname %in% adv_name_list)
three_process$adv_num="0"
three_process$adv_num[three_process$sname==one_name]="1"
three_process$adv_num[three_process$sname==two_name]="2+"
three_process$adv_num=as.factor(three_process$adv_num)

max_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),max)
min_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),min)

#Truncation
three_process = rbind(subset(
  three_process,(sname == max_time$Group.1[1] &three_process$grids >=min_time$x[1]-0.2&
                   three_process$grids <= max_time$x[1]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[2] &three_process$grids >=min_time$x[2]-0.2&
            three_process$grids <= max_time$x[2]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[3] &three_process$grids >=min_time$x[3]-0.2&
            three_process$grids <= max_time$x[3]+0.2)
  ))

eigen_data=data.frame(age_grids=rep(age_grids,2),
                      eigen=c(eigen_1,eigen_2),
                      eigen_down=c(eigen_1_down,eigen_2_down),
                      eigen_up=c(eigen_1_up,eigen_2_up),
                      id=as.factor(rep(c(1,2),each=length(age_grids))))
p_eigen=ggplot()+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen,col=id))+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_up,col=id),lty=2)+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_down,col=id),lty=2)+
  scale_color_brewer(palette="Set1", labels=paste0(c("1st  PC:","2nd PC:"),format(100*var_explain,digits=4),"%"))+
  labs(x = "Age at sample collection",
       y = "Eigenfunction",
       color="")+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_traj=ggplot() +
  geom_line(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  #  geom_point(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  geom_line(linetype = 2, lwd=0.8,data=three_process,aes(x=grids, y=process.val,color=factor(adv_num)))+
  scale_color_brewer(palette="Dark2", labels=paste0(adv_name_list,c(":0",":1",":2+")))+
  labs(x = "Age at sample collection",
       y = "Social integration residuals",
       color="Individual name")+ylim(min(three_residuals$resids)-0.1,max(three_residuals$resids)+0.1)+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_ordination=ggplot(pca_scores_gcs_cumadversities, aes(score_1, score_2, color=adv_cum_asfactor))+
  geom_point(size=2.0)+
  stat_ellipse(size=1.0)+
  scale_color_brewer(palette="Dark2", labels=c("0", "1", "2+"))+
  # geom_label(data=data.frame(name=c("Q1","Q2","Q3","Q4"),
  #                          x=c(min(score_1)+1,max(score_1)-1,max(score_1)-1,min(score_1)+1),
  #                          y=c(2,2,-2,-2)),aes(label=name,x=x,y=y))+
  geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%adv_name_list),
             aes(label=sname),show.legend= F,hjust=-0.5,vjust=0.5)+
  # geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%name_combined),
  #            aes(label=sname))+
  geom_vline(xintercept = median(score_1))+
  geom_hline(yintercept = median(score_2))+
  # xlim(-0.4,0.4)+
  #ylim(-1,3)+
  # ylim(quantile(pca_scores_gcs_cumadversities$score_2,0.01),quantile(pca_scores_gcs_cumadversities$score_2,0.99))+
  # xlim(quantile(pca_scores_gcs_cumadversities$score_1,0.01),quantile(pca_scores_gcs_cumadversities$score_1,0.99))+
  labs(x="Score on PC 1", y="Score on PC 2", color="Number of adversities")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.02, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
####################################################
pdf("../../Figures/eigen_function/SCI_F_eigen.pdf")
p_eigen
dev.off()

pdf("../../Figures/Ordination/SCI_F_ordination.pdf")
p_ordination
dev.off()

pdf("../../Figures/Individual_Trajectory_Subgroup_Comparison/SCI_F_trajectory.pdf")
p_traj
dev.off()

pdf("../../Figures/Collect_Figures_Eigen_ordination_individual_together/Collected_SCI_F.pdf",height=14,width=8)
grid.arrange(p_eigen,p_ordination,p_traj,nrow=3)
dev.off()





load("plot_SCI_M_pc.RData")
#########################################################
snames_cumadversities <- distinct(Complete.data, sname, adv_cumulative)
pca_scores_gcs <- data.frame(score_1, score_2, Treatment_Ind,sname=unique(Complete.data$sname))
pca_scores_gcs_cumadversities <- data.frame(pca_scores_gcs, snames_cumadversities)
pca_scores_gcs_cumadversities$adv_cum_asfactor <- as.factor(pca_scores_gcs_cumadversities$adv_cumulative)
levels(pca_scores_gcs_cumadversities$adv_cum_asfactor) <- list(zero=(0), one=(1), twoplus=c(2,3,4,5))


Residuals=unlist(residuals)
process_mean=process_value_fitted
Complete.data$resids <-Residuals
Complete.data$resids <- as.numeric(Complete.data$resids)

#Need to save process data into new data frame with sname in it

Process.data_gcs=data.frame(
  sname=rep(name.list,each=length(age_grids)),
  grids=rep(age_grids,length(name.list)),
  process.val=as.vector(process_mean))

three_residuals <- subset(Complete.data, sname %in% adv_name_list)
three_residuals$adv_num="0"
three_residuals$adv_num[three_residuals$sname==one_name]="1"
three_residuals$adv_num[three_residuals$sname==two_name]="2+"
three_residuals$adv_num=as.factor(three_residuals$adv_num)

three_process <- subset(Process.data_gcs, sname %in% adv_name_list)
three_process$adv_num="0"
three_process$adv_num[three_process$sname==one_name]="1"
three_process$adv_num[three_process$sname==two_name]="2+"
three_process$adv_num=as.factor(three_process$adv_num)

max_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),max)
min_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),min)

#Truncation
three_process = rbind(subset(
  three_process,(sname == max_time$Group.1[1] &three_process$grids >=min_time$x[1]-0.2&
                   three_process$grids <= max_time$x[1]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[2] &three_process$grids >=min_time$x[2]-0.2&
            three_process$grids <= max_time$x[2]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[3] &three_process$grids >=min_time$x[3]-0.2&
            three_process$grids <= max_time$x[3]+0.2)
  ))

eigen_data=data.frame(age_grids=rep(age_grids,2),
                      eigen=c(eigen_1,eigen_2),
                      eigen_down=c(eigen_1_down,eigen_2_down),
                      eigen_up=c(eigen_1_up,eigen_2_up),
                      id=as.factor(rep(c(1,2),each=length(age_grids))))
p_eigen=ggplot()+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen,col=id))+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_up,col=id),lty=2)+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_down,col=id),lty=2)+
  scale_color_brewer(palette="Set1", labels=paste0(c("1st  PC:","2nd PC:"),format(100*var_explain,digits=4),"%"))+
  labs(x = "Age at sample collection",
       y = "Eigenfunction",
       color="")+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_traj=ggplot() +
  geom_line(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  #  geom_point(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  geom_line(linetype = 2, lwd=0.8,data=three_process,aes(x=grids, y=process.val,color=factor(adv_num)))+
  scale_color_brewer(palette="Dark2", labels=paste0(adv_name_list,c(":0",":1",":2+")))+
  labs(x = "Age at sample collection",
       y = "Social integration residuals",
       color="Individual name")+ylim(min(three_residuals$resids)-0.1,max(three_residuals$resids)+0.1)+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_ordination=ggplot(pca_scores_gcs_cumadversities, aes(score_1, score_2, color=adv_cum_asfactor))+
  geom_point(size=2.0)+
  stat_ellipse(size=1.0)+
  scale_color_brewer(palette="Dark2", labels=c("0", "1", "2+"))+
  # geom_label(data=data.frame(name=c("Q1","Q2","Q3","Q4"),
  #                          x=c(min(score_1)+1,max(score_1)-1,max(score_1)-1,min(score_1)+1),
  #                          y=c(2,2,-2,-2)),aes(label=name,x=x,y=y))+
  geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%adv_name_list),
             aes(label=sname),show.legend= F,hjust=-0.5,vjust=0.5)+
  # geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%name_combined),
  #            aes(label=sname))+
  geom_vline(xintercept = median(score_1))+
  geom_hline(yintercept = median(score_2))+
  xlim(-2,2)+
  ylim(-1,3)+
  # ylim(quantile(pca_scores_gcs_cumadversities$score_2,0.01),quantile(pca_scores_gcs_cumadversities$score_2,0.99))+
  # xlim(quantile(pca_scores_gcs_cumadversities$score_1,0.01),quantile(pca_scores_gcs_cumadversities$score_1,0.99))+
  labs(x="Score on PC 1", y="Score on PC 2", color="Number of adversities")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.02, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
####################################################
pdf("../../Figures/eigen_function/SCI_m_eigen.pdf")
p_eigen
dev.off()

pdf("../../Figures/Ordination/SCI_M_ordination.pdf")
p_ordination
dev.off()

pdf("../../Figures/Individual_Trajectory_Subgroup_Comparison/SCI_M_trajectory.pdf")
p_traj
dev.off()

pdf("../../Figures/Collect_Figures_Eigen_ordination_individual_together/Collected_SCI_M.pdf",height=14,width=8)
grid.arrange(p_eigen,p_ordination,p_traj,nrow=3)
dev.off()




load("plot_proprank_pc.RData")
#########################################################
snames_cumadversities <- distinct(Complete.data, sname, adv_cumulative)
pca_scores_gcs <- data.frame(score_1, score_2, Treatment_Ind,sname=unique(Complete.data$sname))
pca_scores_gcs_cumadversities <- data.frame(pca_scores_gcs, snames_cumadversities)
pca_scores_gcs_cumadversities$adv_cum_asfactor <- as.factor(pca_scores_gcs_cumadversities$adv_cumulative)
levels(pca_scores_gcs_cumadversities$adv_cum_asfactor) <- list(zero=(0), one=(1), twoplus=c(2,3,4,5))


Residuals=unlist(residuals)
process_mean=process_value_fitted
Complete.data$resids <-Residuals
Complete.data$resids <- as.numeric(Complete.data$resids)

#Need to save process data into new data frame with sname in it

Process.data_gcs=data.frame(
  sname=rep(name.list,each=length(age_grids)),
  grids=rep(age_grids,length(name.list)),
  process.val=as.vector(process_mean))

three_residuals <- subset(Complete.data, sname %in% adv_name_list)
three_residuals$adv_num="0"
three_residuals$adv_num[three_residuals$sname==one_name]="1"
three_residuals$adv_num[three_residuals$sname==two_name]="2+"
three_residuals$adv_num=as.factor(three_residuals$adv_num)

three_process <- subset(Process.data_gcs, sname %in% adv_name_list)
three_process$adv_num="0"
three_process$adv_num[three_process$sname==one_name]="1"
three_process$adv_num[three_process$sname==two_name]="2+"
three_process$adv_num=as.factor(three_process$adv_num)

max_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),max)
min_time=aggregate(three_residuals$age_sample,by=list(three_residuals$sname),min)

#Truncation
three_process = rbind(subset(
  three_process,(sname == max_time$Group.1[1] &three_process$grids >=min_time$x[1]-0.2&
                   three_process$grids <= max_time$x[1]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[2] &three_process$grids >=min_time$x[2]-0.2&
            three_process$grids <= max_time$x[2]+0.2)),
  subset(three_process,
         (sname == max_time$Group.1[3] &three_process$grids >=min_time$x[3]-0.2&
            three_process$grids <= max_time$x[3]+0.2)
  ))

eigen_data=data.frame(age_grids=rep(age_grids,2),
                      eigen=c(eigen_1,eigen_2),
                      eigen_down=c(eigen_1_down,eigen_2_down),
                      eigen_up=c(eigen_1_up,eigen_2_up),
                      id=as.factor(rep(c(1,2),each=length(age_grids))))
p_eigen=ggplot()+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen,col=id))+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_up,col=id),lty=2)+
  geom_line(data=eigen_data,aes(x=age_grids,y=eigen_down,col=id),lty=2)+
  scale_color_brewer(palette="Set1", labels=paste0(c("1st  PC:","2nd PC:"),format(100*var_explain,digits=4),"%"))+
  labs(x = "Age at sample collection",
       y = "Eigenfunction",
       color="")+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_traj=ggplot() +
  geom_line(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  #  geom_point(data=three_residuals, aes(x=age_sample, y=resids,color=factor(adv_num)))+
  geom_line(linetype = 2, lwd=0.8,data=three_process,aes(x=grids, y=process.val,color=factor(adv_num)))+
  scale_color_brewer(palette="Dark2", labels=paste0(adv_name_list,c(":0",":1",":2+")))+
  labs(x = "Age at sample collection",
       y = "Social integration residuals",
       color="Individual name")+ylim(min(three_residuals$resids)-0.1,max(three_residuals$resids)+0.1)+
  theme_linedraw()+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.60, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))


p_ordination=ggplot(pca_scores_gcs_cumadversities, aes(score_1, score_2, color=adv_cum_asfactor))+
  geom_point(size=2.0)+
  stat_ellipse(size=1.0)+
  scale_color_brewer(palette="Dark2", labels=c("0", "1", "2+"))+
  # geom_label(data=data.frame(name=c("Q1","Q2","Q3","Q4"),
  #                          x=c(min(score_1)+1,max(score_1)-1,max(score_1)-1,min(score_1)+1),
  #                          y=c(2,2,-2,-2)),aes(label=name,x=x,y=y))+
  geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%adv_name_list),
             aes(label=sname),show.legend= F,hjust=-0.5,vjust=0.5)+
  # geom_label(data=subset(pca_scores_gcs_cumadversities, sname%in%name_combined),
  #            aes(label=sname))+
  geom_vline(xintercept = median(score_1))+
  geom_hline(yintercept = median(score_2))+
  xlim(-0.4,0.7)+
  #ylim(-1,3)+
  # ylim(quantile(pca_scores_gcs_cumadversities$score_2,0.01),quantile(pca_scores_gcs_cumadversities$score_2,0.99))+
  # xlim(quantile(pca_scores_gcs_cumadversities$score_1,0.01),quantile(pca_scores_gcs_cumadversities$score_1,0.99))+
  labs(x="Score on PC 1", y="Score on PC 2", color="Number of adversities")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0.02, 0.98),
        legend.background=element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text = element_text(size=20), legend.title = element_text(size=20)) + 
  theme(axis.text.x=element_text(hjust=0.75))
####################################################
pdf("../../Figures/eigen_function/proprank_eigen.pdf")
p_eigen
dev.off()

pdf("../../Figures/Ordination/proprank_ordination.pdf")
p_ordination
dev.off()

pdf("../../Figures/Individual_Trajectory_Subgroup_Comparison/proprank_trajectory.pdf")
p_traj
dev.off()

pdf("../../Figures/Collect_Figures_Eigen_ordination_individual_together/Collected_proprank.pdf",height=14,width=8)
grid.arrange(p_eigen,p_ordination,p_traj,nrow=3)
dev.off()

