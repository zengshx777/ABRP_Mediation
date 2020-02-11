#Order by sname and time
gender.data<-gender.data[order(gender.data$sname,gender.data$age_sample),]
gender.data$lag_gc=gender.data$gc
gender.data$lag_gc_mean=gender.data$gc
id_size_list=aggregate(gender.data$sname,by=list(gender.data$sname),length)$x
end_index=cumsum(id_size_list)
start_index=c(1,end_index+1)[1:length(end_index)]
delete_index=start_index[which(start_index==end_index)]


for (i in 1:length(start_index))
{
  if (i%in%delete_index)
  {
    next
  }
  else{
  start=start_index[i]+1
  end=end_index[i]
  for (j in start:end)
    {
    gender.data[j,"lag_gc"]=gender.data[j-1,"gc"]
    gender.data[j,"lag_gc_mean"]=mean(gender.data[(start-1):j-1,"gc"])
  } 
  }
}

gender.data=gender.data[-delete_index,]



