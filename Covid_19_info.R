library(readxl)
covid <- read_xlsx("/Users/NT/Documents/Project/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/vietnamcovid.xlsx")

head(covid,5)
my_pac <- c("ggplot2","dplyr","tidyverse","Hmisc","waffle","extrafont")
lapply(my_pac, require, character.only = TRUE)
library(dplyr)
library(echarts4r)
library(echarts4r.assets)
library(data.tree)
library(DiagrammeR)
library(igraph)

#Number of covid by time

dt1 <- covid%>%
  group_by(Date)%>%
  count(Date)
dt2 <- covid %>%
  group_by(Date) %>%
  mutate(case_number = max(Patient_no))%>%
  select(Date,case_number) %>%
  unique()

base <- ggplot(data = covid)+
  geom_line(data = dt1, aes(x = Date, y = n))

base +scale_x_datetime(date_breaks = "1 week", date_labels = "%m-%d")+
  geom_text(data = dt1, aes(x = Date, y= n , label = n), vjust = -0.7)

  
#number of case by gender 
dt3 <- covid %>%
  count(Gender) %>%
  mutate(percent = round(n/sum(n)*100,1))
dt3

dt3$path <- c('path://M20.822 19.307c-2.967-.681-6.578-2.437-5.514-4.723.684 1.126 2.801 1.777 4.45.804-4.747-1.204 2.334-9.471-3.871-14.105-1.135-.853-2.526-1.283-3.912-1.283-1.378 0-2.751.425-3.862 1.283-6.206 4.634.876 12.901-3.872 14.105 1.649.974 3.77.293 4.451-.804 1.064 2.286-2.551 4.043-5.514 4.723-2.978.682-3.178 2.466-3.178 4.004l.005.689h23.99l.005-.691c0-1.537-.2-3.32-3.178-4.002z',
              'path://M20.822 18.096c-3.439-.794-6.64-1.49-5.09-4.418 4.72-8.912 1.251-13.678-3.732-13.678-5.082 0-8.464 4.949-3.732 13.678 1.597 2.945-1.725 3.641-5.09 4.418-3.073.71-3.188 2.236-3.178 4.904l.004 1h23.99l.004-.969c.012-2.688-.092-4.222-3.176-4.935z')
dt3 %>%
  e_charts(Gender) %>%
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel= list(show=FALSE)) %>%
  e_y_axis(max =100,
           splitLine = list(show = FALSE),
           axisTick = list(show = FALSE),
           axisLine = list(show = FALSE),
           axisLabel = list(show = FALSE))%>%
  e_pictorial(percent, symbol = path, z =10, name ='covid- infections',
              symbolBoundingData =100, symbolClip = TRUE)%>%
  e_pictorial(percent, symbol = path, name= 'healthy', 
              symbolBoundingData= 100)%>%
  e_color (color = c('#69cce6','gray')) %>%
  e_labels(position = "bottom", offset = c(0,1), 
           textStyle = list(fontSize = 20, fontFamily ='Arial',
                            fontWeight='bold', color = '#69cce6'),
                            formatter = "{@[1]}% {@[0]}")%>%
  e_theme("westeros")

#number of case by nationality

covid$pathString <- paste("source of covid",covid$Source, covid$Related, sep = "/")
source <- as.Node(covid)


source$'Exposed case'$Wuhan$cases <- 2
source$'Exposed case'$cases <- case_count[case_count$Source =="Exposed case",2]
source$''$cases <- case_count[case_count$Source =="Exposed case",2]
print(source, "cases", limit = 5)

case_count[case_count$Source =="Exposed case",2]

covid1<-covid %>%
  select(Source, Related, Patient_no) %>%
  group_by(Source)

covid1$"Country" <- covid1$Related
covid1[covid1$Patient_no==166,2] <-"S.E.A"

covid1$Related <-recode(covid1$Related, "UK" ="EU", "France"="EU", 
       "Germany"="EU", "Malaysia"="S.E.A", "Thailand"="S.E.A", "Cambodia"="S.E.A",
       "Wuhan"="China","Daegu"="Korea", "Spain"="EU", "Indonesia"="S.E.A",
       "Denmark"="EU","thailand"="EU")

covid1$Country <- recode(covid1$Country, "thailand"="Thailand")

covid1$Country<-recode(covid1$Country,"Aus"="","Canada"='', "Japan"='','Russia'="","Unknown"='',"US"="", "EU"="other countries","China"="Wuhan")

source1<- covid1%>%
  filter(Source =="Exposed case") %>%
  group_by(Related) %>%
  count(Country)


source1$pathString <- paste("Exposed source", source1$Related, 
                            source1$Country, source$n, sep ='/' )
source1 <- as.Node(source1)
print(source1, "n")




plot(source1)

plot(as.igraph(source1, directed = TRUE, direction = "climb"))

print(source1, "level")

source1$Get('level', traversal = "pre-order")
source1 <- as.data.frame(source1)
simpleNetwork(source1)

diagonalNetwork(List = source1)

covid1%>%
  filter(Source =="Community transmission case") %>%
  select(Patient_no, Country, Related) %>%
  kable()


covid$Related<- recode(covid$Related,"91125" = "91,125","91127" ="91,127","124151" = "124,151","243250" ="243,250", "BachMai"="Bach Mai")

covid2 <- covid%>%
  filter(Source =="Community transmission case") %>%
  select(Patient_no, Related, Location)

covid2$pathString <- paste("contact tracing", covid2$Related, covid2$Patient_no, sep ="/")
#covid2 <- as.Node(covid2)
#print(covid2) 

covid2$Related <- recode(covid2$Related, "Bach Mai, Ha Loi" ="Ha Loi", "Ha Loi257"= "258", "91,125"="125",
                         "124,151" = "124", "91,127" ="127", "HA Loi243" ="Ha Loi", "Ha Loi243"="Ha Loi",
                         "243,250" ="243", "Bach Mai,133" ="Bach Mai", "BachMai" ="Bach Mai", "91127"="127", "91125"="125", "243250" ="250",
                         "124151" ="124","34,45" ="34","45,48"="48","Ha Lo
                         i257" ="Ha Loi","258"="Ha Loi",
                         "22,23" ="22", "Bach Mai, TS" ="Truong Sinh")
covid2$Related <- recode(covid2$Related, "Ha Loi"= "243")

covid2[covid2$Patient_no =="258",c(1,2,3)]

covid %>%
  filter(Patient_no %in% c(22,23,35,76,286,212,226,240,166,148,183,60,72,1,2,6,146,210))%>%
  select(Patient_no, Related, Location)%>%
  kable()

covid2
covid2 %>%
  group_by(pathString) %>%
  count(pathString) %>%
  kable()



# Case by month

covid3 <- covid %>%
  mutate(month = month(Date))%>%
  group_by(month)%>%
  count(month)
covid3$recover_cases <- c(0,16,55,230,240)
rm(covid3$cases)
colnames(covid3)[2] <- "cases"

covid3<- subset(covid3, select=-recover)
covid3 <- covid3 %>%
  gather("type","case", contains("cases")) 
covid3 <- as.data.frame(covid3)

covid3$type <- recode(covid3$type, "cases" ="Covid cases", "recover_cases"="recover cases")

covid3%>%
  ggplot()+
  geom_bar(aes(x = month, y = case, fill = type), position = 'dodge', stat = 'identity')+
  theme(panel.background = element_blank())+
  theme(axis.line = element_blank(), legend.position = "top")+
  theme(axis.title.y= element_blank())+
  theme(legend.text = element_text(size =10, face = 1), legend.title = element_blank())+
  geom_text_repel(aes(x = month, y = case, label = case), 
            position = position_dodge2(width=1.), hjust =0.5, vjust = -1.0)+
  scale_y_discrete(limits =c(0,300))


covid3 %>%
  ggplot(aes(x = month, y = case))+
  geom_line(aes(color = type, linetype= type))+
  scale_color_manual(values = c("darkred", "steelblue"))

covid3

covid %>%
  mutate(month = month(Date)) %>%
  group_by(month)%>%
  mutate(cases_total = max(Patient_no))%>%
  select(month, cases_total) %>%
  unique()%>%
  kable()

covid$Location <- recode(covid$Location, "Ha Male" = "Ha Nam", "Quang Male"="Quang Nam")
covid %>%
  filter( Source == "Community transmission case") %>%
  group_by (Related) %>%
  count(Related)

#Plot map

library(maps)
library(mapdata)

map.scale(160,-40, relwidth  =0.15, metric = TRUE, ratio = TRUE)

map('worldHires',"Vietnam")

map('worldHires',
    c('Vietnam', 'Hochiminh'))


points(-1.615672,54.977768,col=2,pch=18)

loc <- c("Truc Bach", "Bach Mai", "Truong Sinh", "Phan Thiet", "Ha Loi", "Son Loi", "Bubda")
pat <- c(17,"nan", "nan",34,243,5,91)
city <- c("Ha Noi", "Ha Noi", "Ha Noi", "Binh Thuan", "Ha Noi","Vinh Phuc", "Ho Chi Minh")
case <- c(20,10,25,11,12,6,18)
lat <-c(21.045385,21.001202,20.995840,10.980357,21.163256,21.280121,10.805222)
long <-c(105.841856,105.840750,105.826342,108.259231,105.736736,105.672504, 106.739000)

covid_label <- data.frame(loc, pat, city, case, lat, long)



