


mylist <- list()
for (i in 1:10) {
  df <- i
  mylist <- c(mylist, df)
}
sum(mylist)





x<-c('34-32',67,78,'45-47')
myfxn=function(x){

x<-mean(as.numeric(str_split(x,'-')[[1]]))
return (x)
}

myfxn(x)





myfxn1=function(x){
  y2<-numeric()
 
  for (i in x){
    if (i == as.character(i)){
      print(myfxn(i))
      
    }
    else{
      print(i)
    }
  }
}
myfxn1(x)




myfxn(x)



pangram <- "The quick brown fox jumps over the lazy dog"

x<-strsplit(pangram,' ')
str_length(y)
length(y)
str_count(y)

nchar(y)
length(y)




y<-x[[1]]
unique(toupper(y))

x(c(4,9))

x[c(4,9)]

x[4]

x[4,9]


library(tidyverse)
x<-c('sabin sapkotaeerrtt 38','dhruba gwanwali 32')
nchar(x)

x<-str_sub(x,str_length(x)-2,str_length(x))
x
x1<-as.numeric(str_trim(x))

sum(x1)
myfxn<-function(x){
  for (i in x){
    
  }
}


x<-list(range(10))
x

str_length(c('a','sabin sapkota',NA))
str_c('sabin ','sapkota')
str



str_count('sabin')
str_length('sabin')
x<-"Tostitos Lightly    Salted 175g"
str_sub(x,27)
str_split(x,' ')
str_count(x)
str_sub(x,2,1)

nchar(x)-4
str_length(x)-4


str_split(x,'-')

x<-list('46-78',87,23,'45-34')

x<-c('78-34')
for (i in str_replace(x,'-',',')){
  print(as.integer(i))
}

for (i in strsplit(x,'-')){
  print(i)
}
  

myfxn(x)

dob<-'1980/11/27'
x<-today()-as.Date(dob)
as.duration(x)
now()

ymd('1980/11/27')
ymd(19811127)
year(dob)
month(dob)



library(lubridate)
library(tidyverse)
library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(moments)
time<-c(23,45,78)
skewness(time)

kurtosis(time)

library(e1071)

fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)
str_split(fruits,'and',n=3)








 x<-'34-36'


y<-as.numeric(x)



library(tidyr)
extract_numeric(x)
















dat1<-data.frame(person=c(1:4),
                 treatment=c('t1','t2')
)

dat1



dat2 <- data.frame(
  patient = c(1:4),
  age = c(56, 23, 32, 19),
  gender = c("M", "F", "F", "M")
)

dat2
 merge(
   x=dat1,y=dat2,
   by.x='person',by.y='patient',all=TRUE)




num<-1:4
fac<-as.factor(num)
fac2<-factor(num,labels = c('bad','neutral','god','very good'))


dat<-cars
str(dat)
nrow(dat)
ncol(dat)
colnames(dat)
rownames(dat)

dimnames(dat)
names(dat)

library(dplyr)
sample_n(dat,4,replace=FALSE)

dat[3,]
dat[,2]








x<-c('sabin sapkota','dhuba gwanali','bishal thapaliya')
substr(x,1,3)

strsplit(x,split =',')[3]

x<-c(3,8,9.1,1)
round(cos(x))



x<-'2'
is.character(x)
y<-as.numeric(x)
y

Employee
library(reshape)


starwars %>% filter(skin_color=='light' & eye_color=='brown') %>%  head()

starwars %>%  arrange(desc(height)) %>% head()

starwars %>% slice(1:3)
starwars %>%  slice_head(n=2)


starwars %>%  group_by(species,sex) %>% 
  summarise(height=mean(height, na.rm=T),
            mass=mean(mass,na.rm=T))


mtcars %>%  group_by(vs,am) %>% 
  summarise(n=n()) %>%  group_by(vs) %>% summarise(n=sum(n)) %>% 
  ungroup() %>%  summarise(n=sum(n))
  
  

mtcars %>% group_by(cyl) %>% summarise(disp=mean(disp),
                                       hp=mean(hp)) %>%  filter(disp==max(disp))

mtcars %>%  group_by(cyl) %>%  filter(disp==max(disp))


starwars %>%  mutate(h_m=height/100) %>%  select(h_m,height) %>%  slice_max(h_m,n=3)

my=function(df){
  x<-df %>% rename(weight=mass) %>%  mutate(h_m=height/100,
                     BMI=weight/h_m**2) %>% 
  select(BMI,h_m,everything()) %>% filter(h_m<1.74) %>% slice_max(BMI,n=3)
  return(x)
  }

my(starwars)

starwars %>%  rename(home_worldss=homeworld) %>%  select(home_worldss)

starwars %>%  filter(!is.na(height)) %>% slice_max(height,n=3)

starwars %>% select(ends_with('color'))

starwars %>%  select(home_world=homeworld)

x<-data.frame('id'=101:104,'r'=c(23,76,45,98),
             'python'=c(37,98,56,34),
             'math'=c(76,56,45,90))
library(dplyr)

x  %>% filter(math>45) %>% select(python)

x %>%  group_by(id) %>% filter(c('python','math','r') %in% c(76,98,45,56,90,76))




starwars %>% filter(mass>mean(mass,na.rm=TRUE)) %>% head()

starwars %>% group_by(gender) %>% 
  filter(mass>mean(mass,na.rm=TRUE)) %>% 
  head()




mark<-list(c(47,28,98,76),c(38,89,26,35))
df<-data.frame(mark)
df
colnames(df)<-c('math','science','r','p')
rownames(df)<-c(100,101,102,103,104)
df
df<-NULL
  
id<-c(101:104)
subject<-c('math','science','r','python')
mark<-list(23,74,87,83)
df<-data.frame(id,subject,mark)
df


sol <- substring("Welcome", 3, 5)
print(sol)



list2 <- list(matrix(c(3,9,5,1,-2,8), nrow = 2), c("Jan","Feb","Mar"), list(3,4,5))

for (i in list2[1]){
  for (j in i){
    print(j)
  }
}


a<-c(7,29,98,23)

sort(a, decreasing = TRUE)

a<-c(TRUE,1,8,F)

a

a<-matrix(1:6,nrow=2,ncol=3)
alength(a)
nrow(a)
ncol(a)
rownames(a)<-c('A','B')
colnames(a)<-c('a','b','c')
a
sex<-c('m','m','m')

sex<-factor(sex,levels =c('m','f'))

table(sex)



x.sam <- runif(10000, min = -1, max = 1)
y.sam <- runif(10000, min = -1, max = 1)
joint.sam <- cbind(x.sam, y.sam)

g.indA <- function(point){
  if ((point[1]^2 + point[2]^2) <= 1) return (1.0)
  else return(0)
}

theta.mc <- sum(apply(joint.sam, 1, g.indA))/nrow(joint.sam)
4*theta.mc


theta.mc
x<-runif(1000,-1,1)
y<-runif(1000,-1,1)
c<-cbind(x,y)
library(dplyr)
class(c)
c %>% head() %>% c[x]
c<-cbind(x,y) %>% head() %>% mutate(status=if_else(x**2+y**2<1,1,0))
nrow(c)
str(c)
g<-function(x,y){
  if (x**2+y**2<1){
    return(1)
  }
  return(0)
}

mc<-sum(apply(c,1,g))
mc
4*mc/nrow(c)







seq(from=as.Date('2017/6/1'),
    to=as.Date('2017/7/31'),by='1 week')






rand.unif <- runif(10000, min = -2, max = 0.8)
hist(rand.unif, freq = FALSE, xlab = 'x', density = 20)






my.matrx <- matrix(c(1:10, 11:20, 21:30), nrow = 10, ncol = 3)
my.matrx
apply(my.matrx,2,sum)
apply(my.matrx,1,length)


apply(my.matrx,2,function (x) sd(x)/sqrt(length(x)))







g<-function(x) (cos(50*x)+sin(20*x))**2
integrate(g,0,1)
curve(f,0,1)
n.sam<-1000
x.sam<-runif(n.sam)
theta.mc<-sum(sapply(x.sam,g))/n.sam
print(theta.mc)

































set.seed(0)
h<-function(x) exp(x)
N<-100000
spmresult<-SI.SPM(h,-1,1,exp(1),N)






integrate(f,-Inf,Inf)









f<-function(x) sin(x)/x
integrate(f,-Inf,Inf)[1]

f<-function(x) 1/sqrt(1-x**2)
integrate(f,-1,1)[1]

f<-function(x) 2*sqrt(1-x**2)
integrate(f,-1,1)[1]


f<-function(x) exp(-x**2)
curve(f,-pi,pi,col='red')
paste('ist integral value',integrate(f,-pi,pi)[1])

factorial(5)**2

fxn<-function(n){
c<-2*sqrt(2)/9801
k=0
tot=0
while(k!=n){
  last=myfact(4*k)*(1103+26390*k)/((myfact(k)**4)*(396)**(4*k))
  tot=tot+last
  #print(tot)
  k=k+1
}
print(paste('Value of pi from infnite series',1/(tot*c))) 
}
fxn(42)


factorial(5)
myfxn=function(n){
  i=0
  tot=0
  while(i!=n){
    last= (2**i*(myfact(i))**2)/(myfact(2*i+1))
    tot=tot+last
    i=i+1
    
  }
  #print(tot)
 return (2*tot)
}
myfxn(92)








x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

df<-data.frame(
  'height'=c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131),'weight'=c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
)

print(df)

x<-lm(weight~height,data=df)
summary(x)
predict(weight~height,a)

a <- data.frame(x = 170)
a
rnorm(50)
die <- 1:6
dice <- sample(die, size = 2, replace = F)
dice


library(tidyverse)
library(lubridate)
library(nycflig)

data()
flights
CO2



library(readr)
library(tidyr)
library(dplyr)


source("http://www.ucl.ac.uk/~uctqiax/PUBLG100/setup.R")


dob<-'19801127'
x<-today()-ymd(dob)
as.duration(x)

dseconds(15)
dminutes(10)
dhours(c(12,24))
ddays(0:5)

today()-ddays(1)
today()-dyears(1)

flight %>%  select(dest,time,dist,contains('delay')) %>% head()
flight

flight %>% arrange(dest) %>%  head()

flight<-flight %>% mutate(speed=dist/(time/60)) %>%  head()


flight %>%  group_by(date) %>% summarise(count=n())

flight
flight %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  View()

delay<-flight %>% group_by(dest) %>% 
  summarise(mean=mean(dep_delay))


delays <- flight %>%
  group_by(dest) %>%
  summarise(mean = mean(dep_delay))

delays

delay

now()
flights
dat<-flights
dat
dat<-read.csv('C:\\Users\\sabin\\Desktop\\data\\flights.csv')
names(dat)


dat %>% select(date,hour,minute) %>% head()
dat
dat %>%  select(date.date)
head(dat)

as_date(now())

d1 <- "January 1, 2010"
d2<-mdy(d1)
year(d2)

sw1<-starwars %>% 
  select(name,height,mass,gender) %>% 
  rename(weight=mass) %>% 
  na.omit() %>% 
  mutate(height=height/100) %>% 
  filter(gender %in% c('masculine','feminine')) %>% 
  mutate(gender=recode(gender,
                       masculine='m',
                       feminine='f')) %>% 
  mutate(size=height>1 & weight>75,
         size=if_else(size==T,'big','small'))

library(tidyverse)
library(stringr)

x<-'this is \n sabin'
print(str_length(x))
typeof(str_c('sabin','sapkota',sep=':'))


x<-c('mango','apple','banana','pearl')
str_split(x)
typeof(x)


str_sub(x,1,3)
str_sub(x,-3,-1)

str_to_upper(x)

str_sub(x,1,1)<-'t'
x
str_sort(x)


x<-'sabin sapkota'
for (i in x){
  print(str_count(i,'i'))
}

x %>% mutate(vowels=str_count(x,'[aeiou]'))
x<-stringr::sentences
x
length(x)

x<-' sabin '
str_length(x)
str_length(str_wrap(x,))
x
str_view(x,'ab')

str_count(x,'a')

dat<-starwars %>% filter(sex %in% c('male','female')) %>% 
  select(height,mass,sex) %>%  rename(weight='mass') %>% 
  mutate(height=height/100) %>% 
  mutate(sex1=recode(sex,male='m',female='f')) %>%    #if you want to add new column use recode
  mutate(size=if_else(height>1 & weight >75,'big','small')) %>% 
  filter(!is.na(size))
  
dat<-starwars %>%  select(height,mass,sex) %>%
  filter(sex %in%  c('male','female')) %>% 
  mutate(sex1=if_else(sex=='male','m','f')) 
dat






#ifelse logical expression
ifelse(2>5,'sabin','dhruba')  #logical expression




dat %>%  group_by(cit) %>% 
  summarise(n())

dat %>% group_by(cit) %>% 
  summarise(mean_size=mean(genome_size,na.rm=TRUE))




dat %>% filter(cit=='plus') %>% select (sample,generation,clade)

dat %>%  mutate(genome_bp=genome_size*1e6) %>%
  filter(!is.na(clade))  %>% head




x<-1e-6
print(x)

dat<-read.csv('Ecoli_metadata.csv')
dat


print(getcwd())

library(dplyr)

dat<-matadata
head(dat)
data()
metadata


dat<-mtcars %>% group_by(cyl) %>% 
  summarise(disp=mean(disp),hp=mean(hp)) %>% 
  filter(disp==max(disp))
dat

dat<-mtcars %>% group_by(cyl) %>% filter(disp==max(disp))

dat<-mtcars %>% group_by(vs,am)

dat<-dat %>% summarise(n=n())
dat


dat <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
dat

dat<-ts(dat)


dat
suppressMessages(library(dplyr))
library(hflights)





install.packages('shades')
f<-function(x) x^2
curve(f,-3,3)
library(DescTools)
Shade(f,breaks=c(-3,3),col='red')
grid()



library(shades)

f<-function(x) exp(-x**2)
curve(f,-pi,pi, col='red')
grid()



?curve



myfxn(sinx)




library(tidyverse)
dat<- data("starwars")
head(dat)
View(starwars)

sw<-starwars %>%  
  select (name,height,mass,gender) %>% 
  rename(weight=mass)


sw1<-starwars %>% 
  select(name,height,mass,gender) %>% 
  rename(weight=mass) %>% 
  na.omit() %>% 
  mutate(height=height/100) %>% 
  filter(gender %in% c('masculine','feminine')) %>% 
  mutate(gender=recode(gender,
                        masculine='m',
                        feminine='f')) %>% 
  mutate(size=height>1 & weight>75,
         size=if_else(size==T,'big','small'))

sw1




starwars %>%  select(gender,mass,height,species) %>% 
  filter(species=='Human') %>% 
  na.omit() %>% 
  mutate(height=height/100) %>% 
  mutate(BMI=mass/height^2) %>% 
  group_by(gender) %>% 
  summarise((mean(mass)))
 

starwars %>% select(name:skin_color) %>% head()

my_matrix<-matrix(1:30,nrow=10,ncol=3)
my_matrix
apply(my_matix,1,sum)
apply(my_matix,2,sum)
apply(my_matix,2,length)
apply(my_matrix,2,function(x) length(x)-1)

st.err <- function(x){
  sd(x)/sqrt(length(x))
}
apply(my_matrix,2,st.err)

my_matrix1<-apply(my_matrix,1:2,function(x) x+3)

my_matrix1
A<-c(1:9)
B<-c(1:12)
C<-c(1:15)
my.lst<-list(A,B,C)
lapply(my.lst, sum)

sapply(my.lst,sum)

x<-c(T,F,0,6)
y<-c(F,T,F,T)
x & y
x|y



library(ggplot2)
dat<-ggplot2::mpg

p <- ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point()

p+scale_x_log10()+scale_y_log10()


p+aes(color=class)+
  theme(legend.title = element_blank(),legend.position = 'bottom')
ggsave('medi.jpg')

p+aes(color=cty>median(cty))
ggsave('medi.jpg')


p + geom_text(aes(label = rownames(dat)),
              check_overlap = TRUE,
              size = 2,
              vjust = -1) +geom_point(color='darkgreen')
  labs(
    title = "Fuel efficiency for 38 popular models of car",
    subtitle = "Period 1999-2008",
    caption = "made in nepal sabin ",
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon (mpg)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5, # center
      size = 12,
      color = "steelblue",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, # center
      size = 10,
      color = "gray",
      face = "italic"
    ))



  
p+geom_smooth(method=lm)
p+aes(color=drv,shape=drv)+
  geom_smooth(method=lm,se=F)
  

p+facet_grid(.~drv)+geom_smooth(method=lm)


p+theme_bw()
install.packages("plotly")
library(plotly)
ggplotly(p+aes(color=year))
ggsave('sabin.pdf')

x1<-c()
x1<-c(x1,3)
x1<-c(x1,8)
sum(x1)

for (i in 1:6){
  print(i)
}


myfxn=function(n){
  v=c()
  for (i in 1:(n-1))
    {
    
    if (n%%i==0)
      {
      v<-c(v,i)
      
    }
  }
  if (sum(v)==n){
    return (n)
  }
  else
  {
    return ('its not perfect square')
  }
  }
  
  myfxn(96)
  
  

for (i in 1:30)
  {
  print(myfxn(i))
}

  
  
  
x<-1:7
x
seq(1,3,by=0.2)
x[x<4]<-5
x
x<-character()
append(x,4,1)
x


x<-c(6,7)
append(x,1:3,after=3)
append(x,1,2)
x



x1<-c()
c(x1,1)
x1



values<-1:9
values
class(values)
typeof(values)
str(values)


for( i in values){
  vector<-c(vector,i)
}
print(vector)
  
x<-c(1,2,3,6.7)
x<-as.integer(x)
x

x<-as.character(x)
x
class(x)

c<-c('sanom',4,TRUE)
c

install.packages(c("devtools", "flexdashboard", "leaflet", "leafpop"))
devtools::install_github("RamiKrispin/coronavirus", force = TRUE)





x<-list(name='sabin', age=26)
x['subject']<-'math'
x
for (i in x){
  print(i)}
  
x[['name']]<-'rawan'
x[1]







#by using for loop
num_dart<-1000000
n_dart_circle<-0
for (i in 1:num_dart){
  x<-runif(1,-1,1)
  y<-runif(1,-1,1)
  if (x**2+y**2<=1){
    n_dart_circle=n_dart_circle+1
  }
}
print(paste('value of pi from monte carlo simuation',4*n_dart_circle/num_dart))

#without using loop

num_dart<-100000
n_dart_circle<-0
x<-runif(num_dart,-1,1)
y<-runif(num_dart,-1,1)
sum<-x**2+y**2
x1<-length(which(sum<=1))
print(paste('value of pi from monte carlo simuation without using loop',x1*4/num_dart))
plot(x,y,col='red')


for (i in 1:1000){
  plot(x[1:i],y[1:i],xlim=c(-1,1),ylim=c(-1,1))
  points(x[i],y[i],col='red')
  Sys.sleep(0.05)
}

i<-0:5
tot=0
for (j in i){
   j1<-j^2 
   print(j1)
  tot=tot+j1
}
print(paste('the sum of square is',tot))



#fibonanci series generation

i=0
n=20
a=0
b=1
print(paste(n,'fibonanci series are:'))
while(i<n){
  c=a+b
  print(a)
  a<-b
  b<-c
  i<-i+1
}



#fibonanci series using function
my=function(n){
  i=0
  a=0
  b=1
  print(paste(n,'fibonanci series are:'))
  while(i<n){
    c=a+b
    print(a)
    a<-b
    b<-c
    i<-i+1
  }
}
my(10)




library(tidyverse)
data()

ggplot(data=BOD,mapping=aes(x=Time,y=demand))+
  geom_point(size=5)+
  geom_line(colour='red')


ggplot(BOD,aes(Time,demand))+
  geom_point(size=3)+
  geom_line(colour='green')

View(CO2)
CO2 %>% 
  ggplot(aes(conc,uptake,
             colour=Treatment))+
  geom_point(size=3,alpha=0.5)+
  geom_smooth(method=lm,se=F)+
  facet_wrap((~Type)) +
  labs(title='sabin of co2')+
  theme_bw()


CO2 %>% 
  ggplot(aes(Treatment,uptake))+
  geom_boxplot()+
  geom_point(alpha=0.5,aes(size=conc,
                 colour=Plant))+
  facet_wrap(~Type)
  coord_flip()+theme_bw()+
    labs(title='sabin drawing')
  
  
library(tidyverse)
data()


dat<-iris
head(dat) 
str(dat)


library(ggplot2)
data()
?mpg

x<-c(23,78,98,11)
x1<-ifelse(x<30,'male','female')
x1



# wallis method
print(paste('Actual pi value', pi))
i=1
n=100000
last=1
while(i!=n){
  c=(4*i^2)/(4*i^2-1)
  last=last*c
  i=i+1
  }
print(paste('pi value calculted from wallis function' ,2*last))

# wallis function using function


myfunc=function(n){
  print(paste('Actual pi value', pi))
  i=1
 
  last=1
  while(i!=n){
    c=(4*i^2)/(4*i^2-1)
    last=last*c
    i=i+1
  }
  print(paste('pi value calculted from wallis function' ,2*last))
}
myfunc(100000)

#Newton method
myfunc=function(x,a){
  # here x in arbitary number and a is number for which we need to find square root
  
  for (i in 1:30){
    print(x)
    y<-(x+a/x)/2
    if(x==y){
      break
    }
    x<-y 
  }
  
}
myfunc(100,pi)

print(factorial(5))

myfact=function(n){
  if (n<0){
    return ('please enter positive value')
  }
  else if (n==1 | n==0){
      return (1)
    }
  else
  { return (n*myfact(n-1))}
  }
myfact(5)

myfunc=function(x,a){
  # here x in arbitary number and a is number for which we need to find square root
  
  while(TRUE){
    print(x)
    y<-(x+a/x)/2
    if(x==y){
      break
    }
    x<-y 
  }
  
}

x<-e-15



myfunc(10,4)


exp(-15)

my=function(n){
  c=(2*sqrt(2))/9801
  i=0
  tot=0
  while(i!=n){
    c1=((myfact(4*i)*(1103+26390i))/((myfact(i)**4)*(396**(4*i))))
    tot=tot+c1
    
    
    
    i=i+1
    
  }
  return (1/(tot*c))
}
my(10)







x <- seq(-pi,pi,0.1)
plot(x, sin(x), main='The sin Function',ylab='sin(x)',type='l',col='blue')
lines(x,cos(x),col='red')
legend('topleft',c('sin(x)','cos(x)'),fill=c('blue','red'))


par()
t<-c(22,27,26,24,23,26,28)
par(mfrow=c(1,2))
barplot(t, main="Barplot")
pie(t, main="Piechart", radius=1)


Temperature <- airquality$Temp
Ozone <- airquality$Ozone
par(mfrow=c(2,2))
hist(Temperature)
boxplot(Temperature, horizontal=TRUE)
hist(Ozone)
boxplot(Ozone, horizontal=TRUE)

barplot(t, col=rainbow(7), main="rainbow")



temp <- c(5,7,6,4,8)
barplot(temp, main="By default")
barplot(temp, col="coral", main="With coloring")


dat<-iris

dat<-data(iris)
library(datasets)
dat<-datasets::iris
head(dat)
str(dat)
min(dat$Sepal.Length)
rng<-range(dat$Sepal.Length)
rng
rng[1]
max(dat$Sepal.Length)-min(dat$Sepal.Length)
rng<-function(x){
  range<-max(x)-min(x)
  return (range)
}
rng(dat$Sepal.Length)
median(dat$Sepal.Length)
quantile(dat$Sepal.Length,0.5)
quantile(dat$Sepal.Length,0.25)
quantile(dat$Sepal.Length,0.75)
IQR(dat$Sepal.Length)
sd(dat$Sepal.Length)
var(dat$Sepal.Length)
m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1<-apply(m1,2,sum)
a_m1
sum(m1)
m <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
ml<-lapply(m,tolower)
ml
m1<-unlist(lapply(m,tolower))
m1

dt<-datasets::cars
dtl<-lapply(dt,min)
dtl
lapply(dat[,1:4],sd)
summary(dat)
by(dat,dat$Species,summary)
cv<-sd(dat$Sepal.Length)/mean(dat$Sepal.Length)
cv
t<-table(dat$Sepal.Length)
t1<-sort(t,decreasing = T)
summary(dat$Species)
dat$size<-ifelse(dat$Sepal.Length<median(dat$Sepal.Length),'small','big')
table(dat$size)
table(dat$Species,dat$size)
prop.table(table(dat$Species,dat$size))


library(ggplot2)
dat<-ggplot2::mpg
?mpg

dat <- transform(dat,
                 cyl = factor(cyl),
                 drv = factor(drv),
                 fl = factor(fl),
                 year = factor(year),
                 class = factor(class)
)
ggplot(dat)+aes(x=displ,y=hwy)+
  geom_line(colour='red')+geom_point()

#histogram
ggplot(dat)+aes(x=hwy)+geom_histogram(bins=sqrt(nrow(dat)))

#density plot
ggplot(dat)+aes(x=hwy,y=..density..)+geom_density()+geom_histogram()

ggplot(dat) +
  aes(x = hwy, color = drv, fill = drv) +
  geom_density(alpha = 0.25)

ggplot(dat) +
  aes(x = "", y = hwy) +
  geom_boxplot()


ggplot(dat)+aes(x=drv,y=hwy)+
  geom_boxplot(varwidth = T)+
  geom_jitter(alpha=0.25,width=0.2)

ggplot(dat) +
  aes(x = drv, y = hwy) +
  geom_boxplot(varwidth = TRUE) + # vary boxes width according to n obs.
  geom_jitter(alpha = 0.25, width = 0.2) +
  facet_wrap(~year)

ggplot(dat) +
  aes(x = drv, y = hwy, fill = drv) + # add color to boxes with fill
  geom_boxplot(varwidth = TRUE) + # vary boxes width according to n obs.
  geom_jitter(alpha = 0.25, width = 0.2) + # adds random noise and limit its width
  facet_wrap(~year) +
  theme(legend.position='none')
ggplot(dat) +
  aes(x = drv, y = hwy, fill = drv) + # add color to boxes with fill
  geom_boxplot(varwidth = TRUE) + # vary boxes width according to n obs.
  geom_jitter(alpha = 0.25, width = 0.2) + # adds random noise and limit its width
  facet_wrap(~year) +
  theme(legend.position='none')+
  scale_fill_manual(values=c('darkred','darkgreen','steelblue'))

ggplot(dat) +
  aes(x = drv) +
  geom_bar()

ggplot(dat) +
  aes(x = drv,fill=drv) +
  geom_bar()+
  theme(legend.position = 'none')

ggplot(dat) +
  aes(x = drv, fill = year) + # fill by years
  geom_bar(position='fill')


ggplot(dat) +
  aes(x = drv, fill = year) + # fill by years
  geom_bar(position='dodge')


p <- ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point(color='darkgreen')

p + labs(
  title = "Fuel efficiency for 38 popular models of car",
  subtitle = "Period 1999-2008",
  caption = "Data: ggplot2::mpg. See more at statsandr.com",
  x = "Engine displacement (litres)",
  y = "Highway miles per gallon (mpg)"
)



 
p <- ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point(color='darkgreen')

p + labs(
  title = "Fuel efficiency for 38 popular models of car",
  subtitle = "Period 1999-2008",
  caption = "Data: ggplot2::mpg. See more at statsandr.com",
  x = "Engine displacement (litres)",
  y = "Highway miles per gallon (mpg)"
) +
  theme(
    plot.title = element_text(
      hjust = 0.5, # center
      size = 12,
      color = "steelblue",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, # center
      size = 10,
      color = "gray",
      face = "italic"
    ))
ggsave("plot1.pdf")


p <- ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point(color='darkgreen')
p + scale_x_continuous(breaks = seq(from = 1, to = 7, by = 0.5)) + # x-axis
  scale_y_continuous(breaks = seq(from = 10, to = 45, by = 5)) # y-axis


dat$date <- as.Date("2020-08-21") - 0:(nrow(dat) - 1)
head(dat$date)
print(nrow(dat))
str(dat$date)

p <- ggplot(dat) +
  aes(x = date, y = hwy) +
  geom_line(colour='darkred')
p

x1<-c(3,5,6,7.8)
x1
x2<-as.character(x1)
x2
is.character(x2)
