#Download and Prepare Raw Data ------

temp_data<-read.table(here("Data",".csv"),header=T,sep=",") #Opens data .csv file directly from the 'Data' file in the directory (see "here" function documentation)
attach(temp_data)

winter_data<-filter((temp_data),Season=="Winter")
spring_data<-filter((temp_data),Season=="Spring")
summer_data<-filter((temp_data),Season=="Summer")
fall_data<-filter((temp_data),Season=="Fall")

loop_data<-read.table(here("Data",".csv"),header=T,sep=",") #Opens data .csv file directly from the 'Data' file in the directory (see "here" function documentation)

#Summary Statistics -----

mean(ST) #Mean Surface temperature
mean(FT) #Mean Fracture temperature
mean(DT) #Mean Difference in temperature (positive values mean surface is warmer than fracture; negative means fracture is warmer than surface)
max(ST)
min(ST)
max(FT)
min(FT)
max(DT)
min(DT)

#Winter

mean(ST$winter_data)
mean(FT$winter_data)
mean(DT$winter_data)
max(ST$winter_data)
min(ST$winter_data)
max(FT$winter_data)
min(FT$winter_data)
max(DT$winter_data)
min(DT$winter_data)

#Spring

mean(ST$spring_data)
mean(FT$spring_data)
mean(DT$spring_data)
max(ST$spring_data)
min(ST$spring_data)
max(FT$spring_data)
min(FT$spring_data)
max(DT$spring_data)
min(DT$spring_data)


#Summer

mean(ST$summer_data)
mean(FT$summer_data)
mean(DT$summer_data)
max(ST$summer_data)
min(ST$summer_data)
max(FT$summer_data)
min(FT$summer_data)
max(DT$summer_data)
min(DT$summer_data)

#fall

mean(ST$fall_data)
mean(FT$fall_data)
mean(FT$fall_data)
max(ST$fall_data)
min(ST$fall_data)
max(FT$fall_data)
min(FT$fall_data)
max(DT$fall_data)
min(DT$fall_data)

#Freeze-Thaw Weathering -----

#We will discuss this once we have looked at the raw data. This requires more complex data processing and I'm not sure it will be necessary.

#Thermal Weathering -----

count(ST_1)
count(ST_2)
max(deltaST)

count(FT_1)
count(FT_2)
max(deltaFT)

#Hysteresis -----

loop_plot<-ggplot(data=loop_data,aes(x=ST,y=FT))+geom_point(aes(size=Time,colour=Month))+theme_bw()+
  geom_path(colour='black',alpha=0.5)+scale_size_continuous(c(1,4))+
  xlab("Surface Temperature (°C)")+ylab("Fracture Temperature (°C)")+
  theme(axis.title.x = element_text(size=14,face='bold'),axis.title.y = element_text(size=14,face='bold'),
        axis.text.x = element_text(size=13,colour='black'),axis.text.y = element_text(size=13,colour='black'),
        legend.title = element_text(face='bold'),legend.text = element_text(size=11))+labs(colour="Month",size="Hour of Day")+
  scale_x_continuous(limits=c(-10,35))+scale_y_continuous(limits=c(-10,35))
loop_plot #Please play around with the settings (e.g., colour, icon shape) to represent different components of the data

#Other analyses (just some examples so you can investigate relationships on your own...) -----

#ANOVA Example

av1<-lm(DT~Season) #Create an ANOVA model which examines whether the difference in temperature between surface and fracture is significantly different between seasons
anova(av1) #Show the results of the ANOVA.
par(mfrow=c(1,1)) #Show one graph at a time
plot(av1) #Show the normality (Q-Q) and homoscedasticity plots to see whether the data meets the assumptions of an ANOVA
shapiro.test(DT) #Tests whether the data is normally distributed. If p<0.05, data is not normally distributed so it doesn't meet the assumptions of an ANOVA.
bartlett.test(DT~State) #Tests whether the data is homoscedastic If p<0.05, data is not homoscedastic so it doesn't meet the assumptions of an ANOVA.
kruskal.test(DT~Season,data=temp_data) #Performs a non-parametric version of an ANOVA. Only use if assumptions of normality and homoscedasticity are not met.

#Linear Dorrelation Example

cor1<-lm(ST~FT,data=temp_data) #Create a linear model representing the relationship between surface and fracture temperature.
summary(cor1) #Show results of the linear model.
plot(cor1) #Plot the linear model results.
shapiro.test(ST) #See above.
shapiro.test(ST) #See above.
bartlett.test(ST~FT) #See above.
cor1_np<-cor.test(ST~FT,method="spearman") #Non-parametric version of linear model for if assumptions of normality and homoscedasticity aren't met.
cor1_np #Outupt of non-parametric linear model.

