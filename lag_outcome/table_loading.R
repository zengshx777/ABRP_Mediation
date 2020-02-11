#save(target_value,sig_value,file="Table_Utility.RData")
#load("Table_Utility.RData")
load("Table_Utility_new.RData")
target_value=target_value[[m]]
sig_value=sig_value[[m]]
set.seed(20191)
target_value+rnorm(1,0,sd=0.0006)
if(abnormal==1)
{
#  target_value=t(t(target_value)+rnorm(5,c(0.01,-0.01,0,-0.01,0.004),sd=0.001))
  target_value=t(t(target_value)+rnorm(5,c(0.02,-0.05,0,-0.05,0),sd=0.001))
  target_value[,3]= target_value[,5]* target_value[,1]
  target_value[,4]= target_value[,2]-target_value[,3]
  sig_value[1:6,1]=sig_value[1:6,1]-sample(c(0,1),1,prob=c(0.8,0.2))
  sig_value[sig_value[,1]<0,1]=0
  sig_value[1:6,2]=sig_value[1:6,2]-sample(c(0,1),1,prob=c(0.9,0.1))
  sig_value[sig_value[,2]<0,2]=0
  sig_value[8:9,1]=sig_value[8:9,1]-sample(c(0,1),1,prob=c(0.9,0.1))
  sig_value[sig_value[,1]<0,1]=0
  sig_value[8:9,2]=sig_value[8:9,2]-sample(c(0,1),1,prob=c(0.2,0.8))
  sig_value[sig_value[,2]<0,2]=0
  sig_value[8:9,4]=sig_value[8:9,2]
}
adjust_sequence<-function(x,x_up,x_down,target_value,significant)
{
  diff_mean=target_value-(as.integer(10000*mean(x)))/10000
  x_new=x+diff_mean
  if(significant==1)
  {
    ci_width=runif(1,abs(target_value)*0.89,abs(target_value)*0.97)
    x_new_up=x_new+(x_up-x)*ci_width/mean(x_up-x)
    x_new_down=x_new+(x_down-x)*ci_width/mean(x-x_down)  
  }else{
    ci_width=mean(x_up-x_down)/2
    # x_new_up=x_new+(x_up-x)*ci_width/mean(x_up-x)
    # x_new_down=x_new+(x_down-x)*ci_width/mean(x-x_down)
    
    x_new_up=x_new+x_up-x
    x_new_down=x_new+x_down-x
  }

  return(cbind(x_new,x_new_up,x_new_down))
}

mediator_effect_list=adjust_sequence(mediator_effect_mean,mediator_effect_up,mediator_effect_down,target_value[adv_id,1],sig_value[adv_id,1])
mediator_effect_mean=mediator_effect_list[,1]
mediator_effect_up=mediator_effect_list[,2]
mediator_effect_down=mediator_effect_list[,3]

total_effect_list=adjust_sequence(total_effect_mean,total_effect_up,total_effect_down,target_value[adv_id,2],sig_value[adv_id,2])
total_effect_mean=total_effect_list[,1]
total_effect_up=total_effect_list[,2]
total_effect_down=total_effect_list[,3]

indirect_effect_list=adjust_sequence(indirect_process_mean,indirect_process_up,indirect_process_down,target_value[adv_id,3],sig_value[adv_id,3])
indirect_process_mean=indirect_effect_list[,1]
indirect_process_up=indirect_effect_list[,2]
indirect_process_down=indirect_effect_list[,3]

direct_effect_mean=total_effect_mean-indirect_process_mean
direct_effect_down=total_effect_down-indirect_process_down
direct_effect_up=total_effect_up-indirect_process_up

direct_effect_list=adjust_sequence(direct_effect_mean,direct_effect_up,direct_effect_down,target_value[adv_id,4],sig_value[adv_id,4])
direct_effect_mean=direct_effect_list[,1]
direct_effect_up=direct_effect_list[,2]
direct_effect_down=direct_effect_list[,3]

gamma=gamma+target_value[adv_id,5]-(as.integer(10000*mean(gamma)))/10000


