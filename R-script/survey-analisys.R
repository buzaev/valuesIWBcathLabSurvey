# clear workspace 
rm(list = ls()) 
library(mise)
mise()
library(gdata) #trim

setwd ("/Users/garrybear/Documents/#UCL-DBA-thesis/overleaf/t2-summative-assessment/R-script")

source ("libraries/my_procedures.R") 
#convert_to_journal_format

#this is redirect console to a file console.txt

dir_output_prefix=paste("outputs/",Sys.Date(), sep="")
dir.create(dir_output_prefix)
plotfilenum=0
dir_plots=(paste(dir_output_prefix, "/", "plots",sep=""))
dir.create(dir_plots)
dir_exports=(paste(dir_output_prefix, "/", "exports",sep=""))
dir.create(dir_exports)
dir_data=paste("data/","2025-04-04", sep="")
subsetName="All respondents"

zz <- file(paste(dir_output_prefix, "/", "exports/console.txt",sep=""), open="wt")
sink(zz,  split=TRUE)


#load and prepare ALL patients table
source ("libraries/dataset_conversion.R")
dsSource <- read.csv(paste(dir_data,'/2025-04-04.csv', sep=""), stringsAsFactors = FALSE)
filename="2025-04-04.csv"
ds=dsSource
ds<-convert_ds(dsSource)

export_data = cbind(ds)
write.csv(export_data,paste(dir_exports,"/d.csv", sep=""))
#rm("ds_all_patients_src")

ds=ds[ds$firstTime==1,]
ds=ds[ds$job!="jobOther",]



# All clean
columnNames <- data.frame(Column = colnames(ds))
source("libraries/descriptives.R")
descriptives=getDescriptives (ds)
latex=convertToLatex(descriptives,"Descriptive Statistics")
cat(latex) # без косячных переводов каретки

tableFactors=getFactorTables (ds)
latex=convertToLatex(tableFactors,"Factors")
cat(latex) # без косячных переводов каретки

# Cronbach
source ("libraries/cronbachAll.R")
psychDs=cronbachAll(ds)
psychDs$w=round(psychDs$w,3)
psychDs$a=round(psychDs$a,3)
psychDs$g=round(psychDs$g,3)
latex=convertToLatex(psychDs,"Factors")
cat(latex) # без косячных переводов каретки



source("libraries/correlations.R")
correlationsAll=getAllCorrelations(ds)
correlationsSelected=getSelectedCorrelations(ds)
latex=createSpearmanLatexTable(ds)
cat(latex)

library(ggplot2) # ВНИМАНИЕ! ОН КОНФЛИКТУЕТ С АЛЬФА psych, запускать после расчетов альфы
library(ggthemes)
theme_set(theme_classic())
library(viridis)
library(ggpubr)

source ("libraries/export_dens_plots.R")
exportDensityPlots (ds, plotfilenum, "All respondents")


# wantValueMore модель классификации что бы хотел сотрудник
# Данные не нормальны:
# Если распределение переменной innovativeBehaviorInventory в группах wantValueMore сильно отклоняется от нормального, тест Крускала–Уоллиса будет более подходящим.
# Если размер выборки в каждой группе небольшой, непараметрический тест устойчивее к выбросам и нарушениям предположений.
# Проведение теста Крускала–Уоллиса

kruskal_test <- kruskal.test(innovativeBehaviorInventory ~ wantValueMore, data = ds)
# Извлечение p-value
p_value_kruskal <- kruskal_test$p.value
# Построение графика с аннотацией p-value
gg=ggplot(ds, aes(x = wantValueMore, y = innovativeBehaviorInventory, fill = wantValueMore)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Убираем подписи на оси X
        axis.ticks.x = element_blank()) +  # Убираем отметки на оси X
  labs(
    title = "IWB and Desired Value Fulfillment",
    x = "Desired Value Fulfillment",
    y = "innovativeBehaviorInventory",
    fill="Desired Value Fulfillment"
  ) +
  annotate(
    "text", x = 1, y = max(ds$innovativeBehaviorInventory), 
    label = paste("Kruskal-Wallis p-value:", round(p_value_kruskal, 4)), 
    hjust = 0, size = 4, color = "blue"
  )
plot(gg)
plotfilename=paste(dir_plots,"/","kruskal036-whatWant-iwb.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()

# if zeroSumMindsetBool=true then he has it
ds$zeroSumMindsetBool=(ds$zeroSumMindset-3.5)/abs(ds$zeroSumMindset-3.5)
ds$zeroSumMindsetBool[ds$zeroSumMindsetBool==1]=0
ds$zeroSumMindsetBool[ds$zeroSumMindsetBool==-1]=1
ds$zeroSumMindsetBool=as.factor(ds$zeroSumMindsetBool)


gg=ggplot(ds, aes(x = zeroSumMindsetBool, y = satisfyValueSafety, fill = as.factor(zeroSumMindsetBool))) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
  stat_compare_means(method = "wilcox")
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Убираем подписи на оси X
  #      axis.ticks.x = element_blank()) +  # Убираем отметки на оси X
  labs(
    title = "Влияние wantValueMore на innovativeBehaviorInventory",
    x = "Zero Sum Mindset",
    y = "innovativeBehaviorInventory"
  ) +
  annotate(
    "text", x = 1, y = max(ds$innovativeBehaviorInventory), 
    label = "", #paste("Kruskal-Wallis p-value:", round(p_value_kruskal, 4)), 
    hjust = 0, size = 4, color = "blue"
  )
plot(gg)
plotfilename=paste(dir_plots,"/","wilcox-zerobool-satisfyValueSafety.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()

gg=ggplot(ds[ds$job!="jobChief",], aes(x = job, y = supportManagerial, fill = job)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
  stat_compare_means(method = "wilcox")
theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Убираем подписи на оси X
  #      axis.ticks.x = element_blank()) +  # Убираем отметки на оси X
  labs(
 #   title = "Влияние wantValueMore на satisfyValueJoy",
    x = "Zero Sum Mindset",
    y = "satisfyValueAccomplishment"
  ) #+
 # annotate(
   # "text", x = 1, y = max(ds$satisfyValueJoy), 
  #  label = "", #paste("Kruskal-Wallis p-value:", round(p_value_kruskal, 4)), 
  #  hjust = 0, size = 4, color = "blue"
#  )

plot(gg)



plotfilename=paste(dir_plots,"/","wilcox-zerobool--.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()
  

wilcoxp(ds,ds$zeroSumMindsetBool)
wilcoxp(ds,ds$talentUseFull)
wilcoxp(ds,ds$sex,"Male","Female")
wilcoxp(ds,ds$job,"jobDoctor","jobNurse")
wilcoxp(ds,ds$job,"jobChief","jobNurse")
wilcoxp(ds,ds$job,"jobChief","jobDoctor")
wilcoxp(ds,ds$talentUseWish)
table(ds$talentUseWish)
wilcoxp(ds,ds$talentUseOpportunity)
table(ds$talentUseOpportunity)

# Check Extreme responce bias.
columns_to_check <- c(
  "ideaGen1", "ideaGen2", "ideaGen3",
  "ideaSearch1", "ideaSearch2", "ideaSearch3",
  "ideaCommunication1", "ideaCommunication2", "ideaCommunication3", "ideaCommunication4",
  "ideaImplementationStart1", "ideaImplementationStart2", "ideaImplementationStart3",
  "ideaInvolvingOthers1", "ideaInvolvingOthers2", "ideaInvolvingOthers3",
  "overcomingObstacles1", "overcomingObstacles2", "overcomingObstacles3", "overcomingObstacles4",
  "innovationOutput1", "innovationOutput2", "innovationOutput3",
  "jobSatisfaction",
  "supportManagerial1", "supportManagerial2", "supportManagerial3", "supportManagerial4", "supportManagerial5",
  "supportOrganizational1", "supportOrganizational2", "supportOrganizational3",
  "talentUseOpportunity",
  "satisfyValueSefRespect", "satisfyValueSafety", "satisfyValueWarmRelations",
  "satisfyValueFulfillment", "satisfyValueAccomplishment", "satisfyValueBeingRespected",
  "satisfyValueBelonging", "satisfyValueJoy"
)
# Подсчет количества экстремальных ответов для каждого респондента
ds$extreme_count <- apply(ds[columns_to_check], 1, function(row) {
  sum(row == 1 | row == 7, na.rm = TRUE)
})


gg <- ggplot(ds, aes(extreme_count))+
  #  geom_histogram(binwidth=10, alpha=0.5, aes(fill=gender, y=..density..))+
  geom_density(alpha=0.4,aes(fill=job)) + 
  geom_vline(aes(xintercept=mean(extreme_count)), color="darkgrey",
             linetype="dashed")+
  # scale_color_brewer(palette="Accent") +
  #scale_fill_manual (values = palette_Re) + #заполнение цветов вручную
  #scale_fill_viridis(discrete = TRUE, option = "D")+ 
  theme_minimal()+
  labs(title="Density plot", 
       subtitle="Extreme answers",
       caption = paste("Subset: ",subsetName, "from file ", filename),
       x="Extreme answers",
       y="Density",
       fill="Job")
plot(gg)
#plotfilename=paste(dir_plots, "/", plotfilenum, "-",subsetName,"-", plotname, ".svg", sep="")
#ggsave(plotfilename, units="in", width=7, height=5, dpi=600 )
savegg(plotfilenum,dir_plots,"extreme_count",subsetName)
plotfilename=paste(dir_plots,"/","extreme_count.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()


gg=ggplot(ds, aes(x = job, y = extreme_count, fill = as.factor(job))) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
  stat_compare_means(method = "kruskal")+
theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Убираем подписи на оси X
  #      axis.ticks.x = element_blank()) +  # Убираем отметки на оси X
  labs(
    title = "Extreme answers bias",
    x = "Job",
    y = "Extreme answers number"
  )

plot(gg)
savegg(plotfilenum,dir_plots,"extreme-answers.svg",subsetName)
plotfilename=paste(dir_plots,"/","extreme-answers.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()
