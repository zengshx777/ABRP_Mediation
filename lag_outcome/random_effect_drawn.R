library(ggplot2)
cluster.name=index_cluster$Group.1
df=data.frame(name=cluster.name,
              mean=apply(cluster_rd_M,2,mean),
              min=apply(cluster_rd_M,2,FUN=function(x){quantile(x,0.025)}),
              max=apply(cluster_rd_M,2,FUN=function(x){quantile(x,0.975)}))

df$name=factor(df$name,levels=df$name[order(df$mean)])
p <- ggplot(df,aes(name,mean,shape=name))

#Added horizontal line at y=0, error bars to points and points with size two
p <- p + geom_hline(yintercept=0) +geom_errorbar(aes(ymin=min, ymax=max), width=0,color="black") + geom_point(aes(size=2)) 

#Removed legends and with scale_shape_manual point shapes set to 1 and 16
p <- p + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=rep(1,12))

#Changed appearance of plot (black and white theme) and x and y axis labels
p <- p + theme_bw() + xlab("Levels") + ylab("")
p <- p+title("Random Effect, DSI_F")
p <- p+ coord_flip()
pdf("rd_m.pdf",width=6,height=6)
p
dev.off()
