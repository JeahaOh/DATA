library(dplyr)
weather= read.csv(file.choose())
length(weather)
nrow(weather)
str(weather) #36881


#결측치 처리
sum(is.na(weather))
weather<-na.omit(weather)
sum(is.na(weather))
head(weather)
summary(weather)

weather$key=1:nrow(weather)            
outdata=select_if(weather, is.numeric) 
str(weather)
str(outdata)

for(i in 1:(ncol(outdata)-1)){
  uppercut=fivenum(outdata[,i])[4]+1.5*IQR(outdata[,i])
  lowercut=fivenum(outdata[,i])[2]-1.5*IQR(outdata[,i])
  out<-filter(outdata, outdata[,i]<=uppercut , outdata[,i]>=lowercut) #14228
}
str(out)


idx = sample(1:nrow(weather), 0.7*nrow(weather) )
train = weather[ idx, ]
test = weather[ -idx, ]
str(train)



ddd<-merge(weather, out, by="key")
str(ddd)

#범주화 : 이산적 변수의 범주화( factor 만 사용), 연속형 변수의 범주화
install.packages("ggplot2")
library(ggplot2)
library(MASS)
str(Cars93)
hist(Cars93$MPG.highway)
disc_1 <- Cars93[,c("Model", "MPG.highway")]
head(disc_1)

disc_1 <- within( disc_1, {  # with(값 변경이 불가), within(값의 변경이 가능)
  MPG.highway_cd = character(0)
  MPG.highway_cd[MPG.highway >= 20 & MPG.highway < 25 ] = "20~25"
  MPG.highway_cd[MPG.highway >= 25 & MPG.highway < 30 ] = "25~30"
  MPG.highway_cd[MPG.highway >= 30 & MPG.highway < 35 ] = "30~35"
  MPG.highway_cd[MPG.highway >= 35 & MPG.highway < 40 ] = "35~40"
  MPG.highway_cd[MPG.highway >= 40 & MPG.highway < 45 ] = "40~45"
  MPG.highway_cd[MPG.highway >= 45 & MPG.highway < 50 ] = "45~50"
  MPG.highway_cd = factor(MPG.highway_cd,  # 숫자, 문자
                          level = c("20~25", "25~30", "30~35", "35~40", "40~45", "45~50"))
  })
attributes(disc_1$MPG.highway_cd)

# 문제 ) 위의 데이터를 MPG.highway 를 기준으로 오름차순으로 정렬하시요 
(disc_1 <- disc_1[order(disc_1$MPG.highway), ])
# 문제 : 범주화된 구간값에 따라 MPG.highway의 합계, 평균, 표준편차를 구하시요 
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sum)  # 합계 - 실행할 함수의 위치 전달.
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, mean)  # 평균
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sd)  # 표준 편차 분산

#문제 : ggplot2를 이용하여 MPG.highway를 범주별로 그라프 출력하시요(dotplot사용) 
library(ggplot2)
# + layer 추가
ggplot(disc_1, aes(x=MPG.highway, fill=MPG.highway_cd)) + geom_dotplot(binwidth=2)
qplot( disc_1$MPG.highway, data=disc_1, color=disc_1$MPG.highway_cd2, binwidth=1 ) 
## 분산이 가장 큰 변수가 가장 큰 역할을 한다.



########## min-max 정규화
search()  # 메모리에 로딩된 패키지 확인
attach(airquality)  # 메모리에 데이터를 패키지 처럼 사용하도록 함.
str(airquality)
transform(Ozone, logOzone = log(Ozone))
search()
detach(airquality)

attach(mtcars)
ls()

# min_max 정규화 (0~1 사이의 값으로 변환) : 동등한 영향력을 부여 ( 종속 변수 )
(if_0_1 <- (mtcars$mpg - min(mtcars$mpg)) / (max(mtcars$mpg) - min(mtcars$mpg))) 
(tf_0_1_2 <- with(mtcars, (mpg - min(mpg) / max(mpg) - min(mpg)) ))


search() 
attach(airquality)
transform(Ozone, logOzone = log(Ozone)) 
search()
detach(airquality)
search()
ls()

(tf_0_1 <- ( mtcars$mpg- min(mtcars$mpg)) / (max(mtcars$mpg) - min(mtcars$mpg)))
(tf_0_1_2 <- with(mtcars, ( mpg- min(mpg)) / (max(mpg) - min(mpg)) ))



iris[,   names(iris) %in% c("Sepal.Length", "Species"),  iris$Species=="setosa"  ]
iris[, ! names(iris) %in% c("Sepal.Length", "Species"),  iris$Species=="setosa"  ]

subset(iris, subset=(Species=="setosa"))
subset(iris, subset=(Species=="setosa" & Sepal.Length>5.0))
subset(iris, select=c(Sepal.Length, Species))
subset(iris, select=-c(Sepal.Length, Species))
subset(iris,  select=c(Sepal.Length, Species), subset=(Species=="setosa") )

# 문제 mtcars 데이터 프레임의 데이터를 이용하여 
attach(mtcars)
# 실린더가 4 또는 6개인 자동차들 중 (연비 mpg, 실린더cyl, 변속기am, 마력hp) 종류를 출력하시요
subset(mtcars, select=c(mpg, cyl, am), subset=(cyl %in% c(4,6)))
mtcars[ which( mtcars$cyl == c(4, 6)), c("mpg", "cyl", "am")]
# subset( mtcars, select = c( mpg, cyl, am ), subset = ( cyl == c(4, 6)))
subset(mtcars, select=c(mpg, cyl, am), cyl>=4 & cyl<=6)
# 자동변속기인 자동차를 선택( 출력 : ("mpg", "cyl", "am"))
subset( mtcars,select = c( mpg, cyl, am ), subset = (am==0))
mtcars[ which( mtcars$am == 0 ), c("mpg", "cyl", "am")]
# 연비(mpg)가 20보다 큰 자동차를 선택하고 mpg, cyl, am을 출력하시요
subset( mtcars,select = c( mpg, cyl, am ), subset = (mpg>=20))
# 변속기가 자동이고 실린더가 4,6개인 자동차들의 연비 평균은 ?
mean(subset(mtcars, select=c(mpg), am==0 & cyl %in% c(4,6))$mpg)
# 평균 마력(mean(mtcars$hp)) 이상인 자동차 들의(mpg, cyl, am, hp) 출력
subset( mtcars,select = c( mpg, cyl, am, hp ), subset = (hp>=mean(hp)))
detach(mtcars) 


################# dplyr  R 고급언어 클래스 베이스
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

head(diamonds)
str(diamonds)

## 어렵게 씀
(df.diamonds_ideal <- filter(diamonds, cut=="Ideal"))# 조건 
head(df.diamonds_ideal)
(df.diamonds_ideal <- select(df.diamonds_ideal, carat, cut, color, price, clarity)) 
head(df.diamonds_ideal)
(df.diamonds_ideal <- mutate(df.diamonds_ideal, price_per_carat = price/carat)) 
head(df.diamonds_ideal)

## 쉽게 씀 
df.diamonds_ideal_chained <- diamonds %>%
  filter(cut=="Ideal") %>%
  select(carat, cut, color, price, clarity) %>%
  #dplyr::(carat, cut, color, price, clarity) %>% # 파일에서 찾아오라고?
  mutate(price_per_carat = price/carat)   # 파생 변수를 생성


(df.disordered_data <- data.frame(num_var = c(2,3,5,1,4)))
head(df.disordered_data)
arrange(df.disordered_data, num_var)  # 정렬할때 사용하는 arrange
arrange(df.disordered_data, desc(num_var)) # desc 

summarize(df.diamonds_ideal, avg_price = mean(price, na.rm = TRUE) )

diamonds %>%                                        
  filter(cut == "Ideal") %>%                        
  ggplot(aes(x=color,y=price)) +                     
  geom_boxplot()


diamonds %>%                                        
  filter(cut == "Ideal") %>%                        
  ggplot(aes(price)) +                            
  geom_histogram() +                                
  facet_wrap(~ color)      

# iris에서 species로 그루핑 하고, Sepal.Length를 meanSepLength 변수에 요악하고
# x축은 Species로 y축은 meanSepLength로 하여 grom_bar 형태로 출력.
data(iris)
str(iris)
iris %>%
  group_by(Species) %>% # 번주형으로 그룹화
  summarize( meanSepLength = mean(Sepal.Length)) %>% # meanSepLength 추가 변
  ggplot(aes(Sepcies, meanSepLength)) + geom_bar(stat = "identity")
  
install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")        # JDBC()함수 제공 

# install.packages(dbplyr)
library(dbplyr)  # DB용
library(DBI)

library(rstudioapi)  # 다이얼 로그 박스
install.packages("nycflights13")
#install.packages("RMySQL")
con <- DBI::dbConnect(RMySQL::MySQL(),
                      dbname = "TT",
                      host = "127.0.0.1",
                      user = "root",
                      password = rstudioapi::askForPassword("root")
)

# DB에 테이블 저장
copy_to(con, nycflights13::flights, "flights", # library 로딩 없이 사용 
        temporary = FALSE, 
        indexes = list(  # 검색을 빠르게 하기 위해 BTREE (balanced tree)로 정렬 
          c("year", "month", "day"), 
          "carrier", "tailnum", "dest")
)

flights_db <- tbl(con, "flights")  # 테이블 => 접속 포인트
flights_db 
flights_db %>% select(year:day, dep_delay, arr_delay)  # dplyr 명령
flights_db %>% filter(dep_delay > 240)
flights_db %>%   group_by(dest) %>%  summarise(delay = AVG(dep_time))

tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  arrange(desc(delay)) %>%
  filter(n > 100)
nrow(tailnum_delay_db)
tailnum_delay_db %>% show_query()
dbDisconnect(con)

data(cars)
str(cars)
head(cars)
plot(cars$speed, type="l")
plot(cars$dist, type="l")
plot(cars, type="l")
plot(cars, type="p")
plot(cars, type="b")
plot(cars, type="b", pch=20)
plot(cars, type="b", pch=22)
plot(cars, type="b", pch="+")
plot(cars, type="b", pch="+", cex=2)
plot(cars, type="l", pch="+", cex=2, lty="dotdash")
plot(cars, main="그래픽", sub="test", col="red", xlab="speed", ylab="distance", 
     ylim=c(0,40),  xlim=c(1,10),type="l")

plot(cars, cex=0.5)
text(cars$speed, cars$dist, pos=1) 


library(ggplot2) 
data(mpg) 
str(mpg) 
head(mpg) 
summary(mpg)
table(mpg$drv) 
qplot(hwy, data=mpg, bins=5)

qplot(hwy, data=mpg, fill=drv, bins=5) 
qplot(displ, hwy, data=mpg, color=drv) 
qplot(hwy, data=mpg, facets=.~drv, binwidth=2)
qplot(hwy, data=mpg, facets=drv~., binwidth=2) 
qplot(hwy, data=mpg, fill=drv, facets=drv~., binwidth=2)


data(mpg) 
str(mpg)
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_bar()
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_dotplot()
ggplot(mpg, aes(x=hwy)) + geom_histogram( binwidth=2)
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_histogram( binwidth=2)
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_density()
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_density(adjust=4)
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_density(adjust=0.25)
ggplot(mpg, aes(x=trans,y=cty)) + geom_boxplot()   



library(ggplot2)

coin<- c(2,2,0,1,1,1,2,2,3,4,1,3,2,3,3,1,2,2,1,2,2)
coin.num = sort(unique(coin))

coin.freq = rep(0, 4)
for(i in 1:length(coin.num)) {
  coin.freq[i] <- length(coin[coin==i-1])
}
coin.rel = round(coin.freq/length(coin), 2)
coin.cum = cumsum(coin.rel)

coin.freq <- data.frame(coin.num, coin.freq)
names(coin.freq)[2] <- paste("val")
coin.freq$type=rep("freq", length(coin.num))
coin.freq

coin.rel = data.frame(coin.num, coin.rel)
names(coin.rel)[2] <- paste("val")
coin.rel$type=rep("rel", length(coin.num))
coin.rel
coin.cum = data.frame(coin.num, coin.cum)
names(coin.cum)[2] <- paste("val")

coin.cum$type = rep("cum", length(coin.num))
coin.cum

(coin.graph = rbind(coin.freq, coin.rel, coin.cum))

ggplot(coin.graph, aes(coin.num, val, group=type, col=type))  + geom_point() + geom_line()
coin.rel.graph = rbind(coin.rel, coin.cum)

ggplot(coin.rel.graph, aes(coin.num, val, group=type, col=type))   +
  geom_point() + geom_line()


library(rAmCharts)
movies <- read.csv("IMDB-Movie-Data.csv")
amPlot(x = movies$Rating, y = movies$Votes, main = "Revenue vs Votes", xlab = "Rating", ylab = "Votes", col = "lightblue", type = "p")

cov(movies$Votes, movies$Rating)
cor(movies$Votes, movies$Rating)
num_movies <- subset(movies, select = c("Year", "Runtime..Minutes.", "Rating", "Votes", "Revenue..Millions.", "Metascore"))

cov(num_movies, use = "complete.obs")
cor(num_movies, use = "complete.obs")

corrplot(cor(num_movies, use = "complete.obs"), method = "circle")

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data  , axistype=1 ,
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            vlcex=0.8
) 


library(mlbench)
data(Ozone)

opar<-par(mfrow=c(1,2))
plot(Ozone$V6, Ozone$V7, xlab="Windspeed", ylab="Humidity", main="Ozone")
plot(jitter(Ozone$V6), jitter(Ozone$V7),xlab="Windspeed", ylab="Humidity", main="Ozone")
par(opar)


plot(cars$dist, type="o", cex=0.5, xlab="speed", ylab="dist")
tapply(cars$dist, cars$speed, mean)
plot(tapply(cars$dist, cars$speed, mean), type="o", cex=0.5, xlab="speed", ylab="dist")


par(mfrow=c(1,1))
m<-lm(dist ~ speed, data=cars) 
plot(cars)
abline(m)
p<-predict(m, interval="confidence") 
head(p)
x<-c(cars$speed, tail(cars$speed, 1), rev(cars$speed), cars$speed[1])
y<-c(p[, "lwr"], tail(p[, "upr"],1), rev(p[, "upr"]), p[, "lwr"][1])
polygon(x,y,col=rgb(.7,.7,.7,.5))


data(iris)
head(iris)
x <- iris$Sepal.Length
hist(x, prob=TRUE, col="skyblue", border="white")
lines(density(x))
rug(iris$Sepal.Length)


library(lattice)
str(airquality) 
xyplot(ozone ~ wind, data=airquality) 
xyplot(ozone ~ wind | month, data=airquality) 
xyplot(ozone ~ wind | month, data=airquality, layout=c(3,2))
convert<-transform(airquality, Month=factor(month))
str(convert)
xyplot(ozone ~ wind | month, data=convert, layout=c(5,1))


example(histogram)
example(densityplot)


histogram(~gcsescore, data=Chem97)
histogram(~gcsescore|score, data=Chem97)
histogram(~gcsescore|factor(score), data=Chem97)

densityplot(~gcsescore | factor(score), data=Chem97, groups=gender)
densityplot(~gcsescore | factor(score), data=Chem97, groups=gender, plot.points=F)
densityplot(~gcsescore | factor(score), data=Chem97, groups=gender, plot.points=F, auto.key=T)  


library(prob)
tosscoin(1) 
tosscoin(4) 
(S=tosscoin(3, makespace='TRUE')) 
S

rolldie(1) 
rolldie(2)
rolldie(2, nsides=4) 
(rolldie(2, nsides=4, makespace="TRUE"))


cards()
cards(jokers="TRUE")
(cards(makespace="TRUE"))

head(cards())
head(1:50, 10)
tail(cards())
tail(1:50, 2)


roulette()
roulette(european="TRUE",makespace="TRUE")


library(prob)
urnsamples(1:3, size=2, replace=TRUE, ordered=TRUE)
urnsamples(1:3, size=2, replace=FALSE, ordered=TRUE)
urnsamples(c("A", "B", "C"), size=2, replace=FALSE, ordered=TRUE)
urnsamples(c("A", "B", "C"), size=2, replace=FALSE, ordered=FALSE)
(s<-tosscoin(2, makespace=TRUE))
s[1:3]
s[c(2,4),]


c<-cards()
subset(c, suit=="Heart")
subset(c, rank %in% 7:9)

rolldie(3)
subset(rolldie(3), X1+X2+X3 > 16)
x<-1:10
y<-8:12


(s=cards(makespace=TRUE))
(a=subset(s, suit=="Heart"))
(b=subset(s, rank %in% 7:9))
a
b
union(a,b)
intersect(a,b)
setdiff(a,b)


plot(seq(-3.2,3.2,length=50),dnorm(seq(-3,3,length=50),0,1),type="l",xlab="",ylab="",ylim=c(0,0.5))
segments(x0 = c(-3,3), y0 = c(-1,-1),x1 = c(-3,3), y1=c(1,1))
text(x=0,y=0.45,labels = expression("99.7% 가 표준편차 3배수안에 3" ~ sigma))
arrows(x0=c(-2,2),y0=c(0.45,0.45),x1=c(-3,3),y1=c(0.45,0.45))
segments(x0 = c(-2,2),y0 = c(-1,-1),x1 = c(-2,2),y1=c(0.4,0.4))
text(x=0,y=0.3,labels = expression("95% 가 표준편차  2배수 안에 2" ~ sigma))
arrows(x0=c(-1.5,1.5),y0=c(0.3,0.3),x1=c(-2,2),y1=c(0.3,0.3))
segments(x0 = c(-1,1),y0 = c(-1,-1),x1 = c(-1,1),y1=c(0.25,0.25))
text(x=0,y=0.15,labels = expression("68% 가 표준편차  1배수안에 1" * sigma),cex=0.9)


set.seed(100)
h_korean <- rnorm(n=1000, mean = 170, sd = 10) 
h_bushman <- rnorm(n=1000, mean = 140, sd = 8)
height <- data.frame(h_korean, h_bushman) 
rm(h_korean, h_bushman)

head(height) 
attach(height)
par( mfrow = c(1,2))
hist(h_korean, freq = TRUE, main = "한국인 키 빈도 히스토그램")
hist(h_korean, freq = FALSE, main = "한국인 키 확률밀도함수 그래프")

hist(h_bushman, freq = TRUE, main = "부시맨 키 빈도 히스토그램")
hist(h_bushman, freq = FALSE, main = "부시맨 키 확률밀도함수 그래프")

height <- transform(height, z2.h_korean = (h_korean - mean(h_korean))/sd(h_korean), 
                    z2.h_bushman = (h_bushman - mean(h_bushman))/sd(h_bushman))

height <- transform(height, z.h_korean = scale(h_korean),z.h_bushman = scale(h_bushman))
hist(height$z.h_korean, freq=TRUE, main="한국인 표준")
hist(height$z.h_bushman, freq=TRUE, main="부시맨 표준 ")


library(moments)
set.seed(1234)
skewness(rnorm(1000))
kurtosis(rnorm(1000))



library(moments)
set.seed(1234)
n.sample <- rnorm(n = 10000, mean = 55, sd = 4.5)
skewness(n.sample) 
kurtosis(n.sample)  


