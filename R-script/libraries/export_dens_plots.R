exportDensityPlots <- function(ds, plotfilenum, subsetName){
  
  
  gg <- ggplot(ds, aes(innovativeBehaviorInventory))+
    #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
    geom_density(alpha=0.4,aes(fill=sex)) + 
    geom_vline(aes(xintercept=mean(innovativeBehaviorInventory)), color="darkgrey",
               linetype="dashed")+
    # scale_color_brewer(palette="Accent") +
    #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
    #scale_fill_viridis(discrete = TRUE, option = "D")+ 
    theme_minimal()+
    labs(title="Density plot", 
         subtitle="Innovative Behavior Inventory",
         caption = paste("Subset: ",subsetName, "from file ", filename),
         x="Innovative Behavior Inventory",
         y="Density",
         fill="Sex")
  plot(gg)
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  savegg(plotfilenum,dir_plots,"sex-innovativeBehaviorInventory",subsetName)
  
  plotfilename=paste(dir_plots,"/","sex-innovativeBehaviorInventory.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
    
  gg <- ggplot(ds, aes(innovativeBehaviorInventory))+
    #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
    geom_density(alpha=0.4,aes(fill=job)) + 
    geom_vline(aes(xintercept=mean(innovativeBehaviorInventory)), color="darkgrey",
               linetype="dashed")+
    # scale_color_brewer(palette="Accent") +
    #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
    #scale_fill_viridis(discrete = TRUE, option = "D")+ 
    theme_minimal()+
    labs(title="Density plot", 
         subtitle="Innovative Behavior Inventory",
         caption = paste("Subset: ",subsetName, "from file ", filename),
         x="Innovative Behavior Inventory",
         y="Density",
         fill="Job")
  plot(gg)
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  savegg(plotfilenum,dir_plots,"job-innovativeBehaviorInventory",subsetName)
  plotfilename=paste(dir_plots,"/","job-innovativeBehaviorInventory.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  
  gg <- ggplot(ds, aes(innovationSupportInventory))+
    #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
    geom_density(alpha=0.4,aes(fill=sex)) + 
    geom_vline(aes(xintercept=mean(innovationSupportInventory)), color="darkgrey",
               linetype="dashed")+
    # scale_color_brewer(palette="Accent") +
    #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
    #scale_fill_viridis(discrete = TRUE, option = "D")+ 
    theme_minimal()+
    labs(title="Density plot", 
         subtitle="Innovative Behavior Inventory",
         caption = paste("Subset: ",subsetName, "from file ", filename),
         x="Innovation Support Inventory",
         y="Density",
         fill="Sex")
  plot(gg)
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  savegg(plotfilenum,dir_plots,"sex-innovationSupportInventory",subsetName)
  plotfilename=paste(dir_plots,"/","sex-innovationSupportInventory.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  gg <- ggplot(ds, aes(innovationSupportInventory))+
    #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
    geom_density(alpha=0.4,aes(fill=job)) + 
    geom_vline(aes(xintercept=mean(innovationSupportInventory)), color="darkgrey",
               linetype="dashed")+
    # scale_color_brewer(palette="Accent") +
    #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
    #scale_fill_viridis(discrete = TRUE, option = "D")+ 
    theme_minimal()+
    labs(title="Density plot", 
         subtitle="Perceived Innovative Behavior Support",
         caption = paste("Subset: ",subsetName, "from file ", filename),
         x="Innovation Support Inventory",
         y="Density",
         fill="Job")
  plot(gg)
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  savegg(plotfilenum,dir_plots,"job-innovationSupportInventory",subsetName)
  plotfilename=paste(dir_plots,"/","job-innovationSupportInventory.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  gg <- ggplot(ds, aes(zeroSumMindset))+
    #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
    geom_density(alpha=0.4,aes(fill=job)) + 
    geom_vline(aes(xintercept=mean(zeroSumMindset)), color="darkgrey",
               linetype="dashed")+
    # scale_color_brewer(palette="Accent") +
    #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
    #scale_fill_viridis(discrete = TRUE, option = "D")+ 
    theme_minimal()+
    labs(title="Density plot", 
         subtitle="Zero Sum Mindset",
         caption = paste("Subset: ",subsetName, "from file ", filename),
         x="Zero Sum Mindset",
         y="Density",
         fill="Job")
  plot(gg)
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  savegg(plotfilenum,dir_plots,"job-zeroSumMindset",subsetName)
  plotfilename=paste(dir_plots,"/","job-zeroSumMindset", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  gg <- ggplot(ds, aes(zeroSumMindset))+
    #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
    geom_density(alpha=0.4,aes(fill=wantValueMore)) + 
    geom_vline(aes(xintercept=mean(zeroSumMindset)), color="darkgrey",
               linetype="dashed")+
    # scale_color_brewer(palette="Accent") +
    #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
    #scale_fill_viridis(discrete = TRUE, option = "D")+ 
    theme_minimal()+
    labs(title="Density plot", 
         subtitle="Zero Sum Mindset",
         caption = paste("Subset: ",subsetName, "from file ", filename),
         x="Zero Sum Mindset",
         y="Density",
         fill="Job")
  plot(gg)
  #plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
  #ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
  savegg(plotfilenum,dir_plots,"wantValueMore-zeroSumMindset",subsetName)
  plotfilename=paste(dir_plots,"/","wantValueMore-zeroSumMindset", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  
  gg=ggplot(ds)+ 
    aes(x=zeroSumMindset,y=innovativeBehaviorInventory)+ #, color=factor(gender)) + #, shape=factor(gender))) + 
    geom_point(size=1, show.legend = TRUE, alpha = .5) +
    #geom_count(show.legend=F) +
    geom_smooth(method="loess", se=F,col="grey", size=0.3) +
    geom_smooth(method="lm", se=TRUE,col="black", size=0.3, level=0.95) +
    #scale_fill_brewer(palette=color_theme) +
    stat_cor(method = "pearson", label.x.npc =0.3, label.y.nrc = 0.30, size=4)+
    stat_ellipse(color="lightgrey")+
    labs(subtitle="", 
         x="zeroSumMindset", 
         y="innovativeBehaviorInventory", 
         title="Scatterplot", 
         fill="Sex",
         caption = paste( "Subset: ",subsetName, "from file", filename ))
  plot(gg)
  
  
}