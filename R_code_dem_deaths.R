#Import libraries
library(openxlsx)
library(dplyr)
library(caret)     
library(psych)
library(corrplot)
library(ggplot2)
library(forcats)
library(viridis)
library(hrbrthemes)
library(lattice)
library(ggthemes)
library(ggpubr)
library(ggrepel)
library(extrafont)
library(ggalt)
library(scales)
font_import()
loadfonts(device = "win")
library(dendextend)
library(ggdendro)
library(countrycode)


# Load given dataset(geo,year,sex,dementia deaths)
data <-read.xlsx("C:\\Users\\postgres\\Desktop\\Visualization\\dementia deaths.xlsx", detectDates = TRUE)
data<-as.data.frame(data)
data <- data[,-5]
data<- data %>% rename('dementia_deaths' = 'OBS_VALUE')
View(data)

# loading a dataset that includes the country,year and total population
pop <- read.csv("C:\\Users\\postgres\\Desktop\\Visualization\\population_sheet.csv", header=TRUE)
pop <- as.data.frame(pop)
pop<- pop[,c('geo','TIME_PERIOD','OBS_VALUE')]
pop<- pop %>% rename('population' = 'OBS_VALUE')
View(pop)

# loading a dataset that includes the age,sex,country,year and dementia deaths
age <- read.csv("C:\\Users\\postgres\\Desktop\\Visualization\\age.csv", header=TRUE)
age <- as.data.frame(age)
age <- age[,c('geo','TIME_PERIOD','sex','age','OBS_VALUE')]
age<- age %>% rename('dementia_deaths_per_age' = 'OBS_VALUE')
View(age)

# loading the dataset that includes the year, the country and the total deaths
deaths<- read.csv("C:\\Users\\postgres\\Desktop\\Visualization\\deaths_sheet.csv",header=TRUE)
deaths <- as.data.frame(deaths)
deaths <- deaths[,c('geo','TIME_PERIOD','OBS_VALUE')]
deaths<- deaths %>% rename('total_deaths' = 'OBS_VALUE')
deaths<-deaths[1:577,]
View(deaths)

################################# Preprocessing  ###########################################
# Adding population, age and total deaths to the given dataset
m1 <- left_join(data,pop, by = c("geo","TIME_PERIOD"))
m2 <- left_join(m1,age, by = c("geo","TIME_PERIOD","sex"))
final <- left_join(m2,deaths, by = c("geo","TIME_PERIOD")) 
final <- final %>% rename('year' = 'TIME_PERIOD')

# Keeping only the records from 2011 to 2020
final<-final[final$year != "2021", ]
range(final$year)

# encoding 
final<-final %>% mutate(sex_num = ifelse(sex=='F', 1, 0))
final<-final %>% mutate(Female = ifelse(sex=='F', 1, 0),Male = ifelse(sex=='M', 1, 0))
final['geo_num'] <- as.numeric(factor(final$geo))
final['geo_num'] <- as.numeric(factor(final$geo))
final['age_num'] <- as.numeric(factor(final$age))
final

# contigency matrix of country and year
table(final$year,final$geo)

#dropping the records of the countries that don't provide data for the time period 2011-2020
final<- subset(final, geo!='LI' & geo!='PT' & geo!='RO' & geo!='SI'& geo!='TR' & geo!='UK' & geo!='EU28') 
final

# Checking for duplicates
final[duplicated(final), ]

# Checking for NaN values
any(is.na(final))
glimpse(final)
# dementia deaths per 100,000 residents
sf <- 100000 
final["dd_per_100K_res"] <- (final$dementia_deaths / final$population) * sf
final["dd_per_100K_res"] <- round(final$dd_per_100K_res,3)
View(final)

# dementia deaths per 1000  deaths
sf_1 <- 1000
final["dd_per_1K_deaths"] <- (final$dementia_deaths /final$total_deaths) * sf_1
final["dd_per_1K_deaths"] <- round(final$dd_per_1K_deaths,3)
View(final)

# Print Greece
final[final$geo == "EL", ]
final[order(final$year), ]
final['year_fact'] <- as.factor(final$year)


# Dataframe without gender 
final_no_gender <- final %>% group_by(geo,year)  %>%summarise(dementia_deaths = sum(dementia_deaths),
                                                              total_deaths=unique(total_deaths),population=unique(population),.groups = 'drop')
final_no_gender['dd_per_100K_res'] <- (final_no_gender$dementia_deaths / final_no_gender$population) * sf
final_no_gender["dd_per_1K_deaths"] <- (final_no_gender$dementia_deaths /final_no_gender$total_deaths) * sf_1
View(final_no_gender)
# Dementia deaths per age and gender dataframe
final$age <- factor(final$age, levels = c("Y_LT65", "Y65-74", "Y75-79","Y80-84","Y85-89","Y90-94","Y_GE95"), ordered = TRUE)
dd_per_age_gender<-as.data.frame(aggregate(dementia_deaths_per_age~sex+age, data=final, FUN=sum))

# violin plot
el_eu27<-subset(final_no_gender,geo == 'EL'| geo == 'EU27_2020')

vp<-ggplot(el_eu27, aes(x = geo, y = dd_per_100K_res)) + geom_violin(aes(fill = geo),trim=FALSE)+geom_boxplot(width=0.2) + labs(title = "Dementia Mortality Rates: Greece vs. Rest of Europe",subtitle="Dementia Deaths per 100K residents",x = "",y = "") + scale_x_discrete(labels = c("Greece", "Rest of Europe")) + scale_fill_manual(labels = c("Greece", "Rest of Europe"), values = c("lightblue", "orange1"), name = "Country")+ theme_tufte()+theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5))


# trelis plot 
w<-subset(final_no_gender,geo!='EU27_2020')
trpl<-ggplot(w, aes(x=year, y = dd_per_100K_res)) +
  geom_line(color="blue") +geom_point(color="red",size=0.5) +
  facet_wrap(~geo,nrow=4,ncol=7) + theme_few() +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
         axis.text.y = element_text(size = 6, family = "Times New Roman"),
         plot.title = element_text(hjust = 0.5, family = "Times New Roman",face="bold"),
         text = element_text(family = "Times New Roman")) +
  labs(title = "Dementia deaths per 100K residents per country",
       x = "",y = "") + scale_x_continuous(breaks = seq(2011, 2020, by = 1),
                                           labels = seq(2011, 2020, by = 1)) + scale_y_continuous(limits = c(0, 500))+theme(plot.title = element_text(hjust = 0.5))

# barplot dementia deaths per country
a<-as.data.frame(aggregate(dementia_deaths~geo, data=subset(final,geo!='EU27_2020'), FUN=sum))

options(scipen = 999)
bar<-ggplot(a, aes(x=geo, y=dementia_deaths)) + 
  geom_bar(stat="identity", position=position_dodge(),fill = ifelse(a$geo == "EL", "brown1", "gray"))+geom_text(aes(label = dementia_deaths),position = position_dodge(0.9),hjust = -0.1,size=2.5,angle = 90,family = "Times New Roman")+ labs(title = "Dementia deaths per country",x = "",y = "")+scale_y_continuous(limits = c(0, 3000000), breaks = seq(0, 3000000, by = 1000000),labels = function(x) paste0(x/1e6, "M"))+theme(axis.text = element_text(size = 6),panel.grid.major = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                                                                     panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'white', color = 'gray'))+ scale_x_discrete(guide = guide_axis(n.dodge=2))+ geom_hline(yintercept=c(1000000, 2000000, 3000000), linetype='dashed',colour="salmon",alpha=0.5)+theme_few()+theme( axis.text.x = element_text(angle = 90, hjust = 1, size = 8,face='bold', family = "Times New Roman"),axis.text.y = element_text(size = 8,face='bold', family = "Times New Roman"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               plot.title = element_text(hjust = 0.5, family = "Times New Roman",face="bold"),text = element_text(family = "Times New Roman"))

# box plot dementia deaths per 1K deaths by country and gender 
data_filt <- final %>% filter(geo %in% c("EL","EU27_2020"))

bp<-ggplot(data_filt, aes(x =geo, y = dd_per_1K_deaths, fill = sex)) + geom_boxplot() + 
  ggtitle("Dementia Deaths per 1K Deaths: Greece vs. Rest of Europe",subtitle='Grouped by Gender') + xlab("") + 
  ylab("")+ scale_x_discrete(labels = c("Greece", "Rest of Europe"))+ scale_fill_manual(labels = c("Women", "Men"), values = c("salmon", "dodgerblue"), name = "Gender") +  theme_tufte()+theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5))

# density plot dementia deaths per 1K deaths by country and gender
dens<-ggplot(data_filt, aes(x=dd_per_1K_deaths,fill=sex)) +
  geom_density(alpha=0.5) +
  facet_wrap(~geo, ncol=2, labeller = labeller(geo = c(EL = "Greece", EU27_2020 = "Rest of Europe")), strip.position = "top") + 
  ggtitle("Dementia Mortality Rates Distribution: Greece vs. Rest of Europe ",subtitle="Dementia Deaths per 1K Deaths, grouped by Gender") +
  xlab("") + scale_fill_manual(values = c("salmon", "dodgerblue"), labels = c("Women", "Men") ,name = "Gender")+
  ylab("")+ylim(0, 0.25)+ scale_x_continuous(breaks = seq(-30, 30, by = 10), limits = c(-35, 35))+ geom_hline(yintercept=c(0.05, 0.1, 0.15, 0.2, 0.25), linetype='dashed',colour="gray",alpha=0.4)+ theme_tufte()+theme(plot.title = element_text(hjust = 0.5,face='bold'), plot.subtitle = element_text(hjust = 0.5))

# bubble plot
b<-subset(final_no_gender, geo != "EU27_2020")%>%group_by(geo)%>%summarise(dementia_deaths = sum(dementia_deaths),total_deaths=sum(total_deaths),population=mean(population),.groups = 'drop')

scpl1 <- ggplot(b, aes(x = total_deaths, y = dementia_deaths, label = geo, size = population)) +
  geom_point(color = ifelse(b$geo == "EL", "red", "darkgray"), alpha = ifelse(b$geo == "EL", 1, 0.6), show.legend = TRUE) +
  scale_size(range = c(1, 8), breaks = seq(0, 80000000, by = 10000000), labels = function(x) paste0(x / 1000000, "M")) +
  xlab("Total deaths") +
  ylab("Dementia Deaths") +
  ggtitle("Dementia deaths VS Total deaths", subtitle = "by population and country") +
  scale_y_continuous(limits = c(0, 3000000), breaks = seq(0, 3000000, by = 500000), labels = function(x) paste0(x / 1e6, "M")) +
  scale_x_continuous(limits = c(0, 10000000), breaks = seq(0, 10000000, by = 1000000), labels = function(x) paste0(x / 1e6, "M")) +
  theme_few() +
  geom_rect(data = b, aes(xmin = 0, xmax = 3000000, ymin = 0, ymax = 1000000), fill = NA, color = "dodgerblue", size = 0.5, linetype = "dashed") +
  geom_text_repel(nudge_y = 203000, size = 2, family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", family = "Times New Roman"),
    axis.title = element_text(size = 10, family = "Times New Roman"),
    plot.subtitle = element_text(hjust = 0.5, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman"))

scpl2<-ggplot(b, aes(x = total_deaths, y = dementia_deaths, label = geo, size = population))+
  geom_point(color = ifelse(b$geo == "EL", "red", "darkgray") ,alpha=ifelse(b$geo == "EL", 1,0.6))+
  scale_size(range = c(1, 8), breaks = seq(0, 80000000, by = 10000000), labels = function(x) paste0(x / 1000000, "M")) + 
  geom_text_repel(nudge_y = 37800, size = 2,family="Times New Roman") +xlab("Total deaths") +ylab("Dementia Deaths") +ggtitle("Focusing on the cluttered part" ) +
  scale_y_continuous(limits = c(0, 700000), breaks = seq(0,  700000, by = 100000),labels = function(x) paste0(x/1e6, "M")) + 
  scale_x_continuous(limits = c(0, 1600000), breaks = seq(0, 1600000,by = 200000),labels = function(x) paste0(x/1e6, "M")) + theme_few()+guides(size = FALSE)+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold", family = "Times New Roman"),axis.title = element_text(size = 10, family = "Times New Roman"),plot.subtitle = element_text(hjust = 0.5, family = "Times New Roman"),legend.text = element_text(family = "Times New Roman"),axis.text = element_text(family = "Times New Roman"),legend.title = element_text(family = "Times New Roman"))

scpl_combo <- ggarrange(scpl1, scpl2, ncol = 1,nrow=2,common.legend = TRUE, legend = "right")+ theme(plot.margin = margin(20, 30, 30, 20))

# heatmap dd per age and country
gaddpa<- subset(final, geo != "EU27_2020")%>%group_by(geo,age)%>%summarise(dementia_deaths_per_age = sum(dementia_deaths_per_age),.groups = 'drop')

dummies_age <- model.matrix(~ age-1, data = gaddpa)
dummies_geo <- model.matrix(~ geo-1, data = gaddpa)
dementia_deaths<-gaddpa$dementia_deaths_per_age
gaddpa_numeric <- cbind(dementia_deaths, dummies_geo)

hm<-ggplot(gaddpa, aes(x =age , y = geo, fill = dementia_deaths)) + geom_tile() +
  labs(x = "", y = "", title = "Dementia Deaths",subtitle="by age group and country") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5,size=10)) +scale_fill_viridis_c(option = "magma", labels = label_number_si(scale = 1e-3, suffix = "K"),name="Dementia \n Deaths")+theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman",face="bold"),plot.subtitle = element_text(hjust = 0.5, size = 10, family = "Times New Roman"),axis.text = element_text(family = "Times New Roman"),axis.title = element_text(family = "Times New Roman"),legend.text = element_text(family = "Times New Roman"),
                                                                                                                                                                                                                                                                                                                      legend.title = element_text(family = "Times New Roman"))

#Dumbbell plot prc difference dd_per_pop 2011 VS 2020

test<-subset(final_no_gender[,c(1:3,5)], year==2011 | year==2020)
test['dd_per_pop']<- (test$dementia_deaths/test$population)
test_2011<-subset(test,year==2011 & !(geo %in% c("EU27_2020")))
test_2020<-subset(test, year == 2020 & !(geo %in% c("EU27_2020")))

dbl<-ggplot(subset(test, geo!="EU27_2020"))+geom_segment(data=test_2011,aes(x=geo,xend=geo,y=dd_per_pop,yend=test_2020$dd_per_pop),color='darkgray',alpha=1)+coord_flip()+geom_point(aes(x=geo,y=dd_per_pop,color=factor(year)))+ scale_color_manual(values=c('skyblue','forestgreen'),labels=c('2011','2020'),name='year')+ geom_hline(yintercept=c(0.001, 0.002, 0.003, 0.004,0.005), linetype='dashed',colour="salmon",alpha=0.5)+ labs(x= NULL, y="change (%)", title="Dementia Deaths per Country Population",subtitle="Change (%): 2011 vs 2020")+
  scale_y_continuous(labels = percent_format())+theme_tufte()+theme(plot.title = element_text(hjust = 0.5,face='bold'),plot.subtitle = element_text(hjust = 0.5)) 

#Diverging plot

age_1<-subset(age, geo!='LI' & geo!='PT' & geo!='RO' & geo!='SI'& geo!='TR' & geo!='UK' & geo!='EU28')
age_1<-age_1[age_1$TIME_PERIOD != "2021", ]
age_1<-age_1[age_1$sex!='T', ]
age_1$age[age_1$age == "Y_GE95"] <- "Y95+"

age_2<-subset(age_1, geo!='EU27_2020'& geo!='EL')
age_2<-age_2 %>% group_by(sex,age)  %>% summarise(dementia_deaths = mean(dementia_deaths_per_age),.groups = 'drop')
age_2<-age_2%>% rename(sex=1,age = 2, dementia_deaths_per_age = 3)

age_2_f<- age_2%>% filter(sex == "F")
age_2_m<- age_2%>% filter(sex == "M")

options(scipen = 999) 
d_gr<-ggplot(subset(age_1, geo=='EL'), aes(y = age, x = dementia_deaths_per_age, fill = sex)) +
  geom_col(data = . %>% filter(sex == "F"), position = "dodge",width = 0.4) +
  geom_col(data = . %>% filter(sex == "M") %>% mutate(dementia_deaths_per_age = -dementia_deaths_per_age), position = "dodge",width = 0.4) + scale_fill_manual(values = c("salmon", "skyblue"),labels = c("Women", "Men"),name='Gender') +
  labs(subtitle="Greece", fill = "Sex", x = "", y = "")+scale_x_continuous(breaks = seq(-1500,1500,by=500),limits =c(-1500,1500),labels = abs(seq(-1500,1500,by=500))) +theme_tufte() 

d_eu<- ggplot(age_2, aes(y = age, x = dementia_deaths_per_age, fill = sex)) +
  geom_col(data = age_2_f, position = "dodge", width = 0.4) + 
  geom_col(data = . %>% filter(sex == "M") %>% mutate(dementia_deaths_per_age = -dementia_deaths_per_age), position = "dodge", width = 0.4) +
  scale_fill_manual(values = c("salmon", "skyblue"), labels = c("Women", "Men"), name = 'Gender') +
  labs( subtitle = "Rest of Europe", fill = "Sex", x = "", y = "") +
  scale_x_continuous(breaks = seq(-1500, 1500, by=500), limits = c(-1500,1500), labels = abs(seq(-1500, 1500, by=500))) +  theme_tufte()

div_combo <- ggarrange(d_gr, d_eu, ncol = 1,nrow=2)
div_combo<- annotate_figure(div_combo , top = text_grob("Dementia deaths by age and gender",color = "black",face='bold', size = 14, family='Times New Roman'))

#Dendrogram

z<-subset(final_no_gender,geo != 'EU27_2020')
years <- unique(z$year)
data_list <- lapply(years, function(yr) {
  subset(z, year == yr, c(1,2,3))})

data_matrix_list <- list()

for (i in seq_along(data_list)) {
  data_subset <- data_list[[i]]
  data_matrix <- matrix(data_subset[[3]])
  rownames(data_matrix) <- data_subset[[1]]
  colnames(data_matrix) <- paste('dementia_deaths', unique(data_subset[[2]]), sep='_')
  data_matrix_list[[i]] <- data_matrix
}

combined_mat <- do.call(cbind, data_matrix_list)
combined_mat <- cbind(geo = rownames(combined_mat), combined_mat)
rownames(combined_mat) <- 1:nrow(combined_mat)
combined_mat<- data.frame(combined_mat)%>%mutate_at(vars(-1), as.numeric)

dist_matrix <- dist(combined_mat)
hc <- hclust(dist_matrix,"complete")
dendro <- as.dendrogram(hc)
labels <- combined_mat$geo
labels <- labels[rev(order.dendrogram(dendro))]
dendro <- set(dendro, "labels", labels)
dendro <- ggdendrogram(dendro)

dend<- dendro + xlab("") + ylab("") + ggtitle("Hierarchical Clustering of Countries:",subtitle="Based on Dementia Deaths per Year")+theme_tufte()+ theme(axis.text.x = element_text(color = c(rep("black", 16), "red", rep("black", 1)), face = c(rep("plain", 16), "bold", rep("plain", 1))),plot.title = element_text(hjust = 0.5,face="bold"),plot.subtitle = element_text(hjust = 0.5),axis.text.y = element_text(angle = 0, vjust = 0.5)) + guides(fill = guide_legend(reverse = TRUE))+scale_y_continuous(labels = function(x) formatC(x/1000, digits = 0, format = "f")  %>% paste0("K"))

dend_zoom<-dend+ xlab("Countries") + ylab("Cluster Distance") + ggtitle("Zooming in",subtitle='')+ theme(axis.text.x = element_text(color = c(rep("black", 16), "red", rep("black", 1)), face = c(rep("plain", 16), "bold", rep("plain", 1))),plot.title = element_text(hjust = 0.5,face="bold"),axis.text.y = element_text(angle = 0, vjust = 0.5)) + guides(fill = guide_legend(reverse = TRUE))+coord_cartesian(ylim=c(0,10000))+ annotate("rect", xmin=15.5, xmax=24.5, ymin=0, ymax=8000, alpha=0.2, fill="salmon")

dend_combo<-ggarrange(dend, dend_zoom, ncol = 1,nrow=2)

#saving
save_plots <- function(plots, filenames, dpi = 300, width = 6, height = 6) {
  if (length(plots) != length(filenames)) {
    stop("The number of plots and filenames must be the same.")
  }
  
  for (i in seq_along(plots)) {
    ggsave(filename = filenames[i], plot = plots[[i]], dpi = dpi, width = width, height = height)
  }
}
plots <- list(vp,trpl,bar,bp,dens,scpl_combo,hm,dbl,div_combo,dend_combo)
filenames <- c("violin_plot.png", "trellis_plot.png","barplot.png","boxplot.png","density_plot.png","bubbleplot.png","heatmap.png","dumbbell.png","divergent.png","dendrogram.png")

save_plots(plots, filenames, dpi = 300, width = 6, height = 6)
