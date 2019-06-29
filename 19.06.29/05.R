# 카이 제곱 분석
# 독립 변수 / 종속 변수
# 범주형 / 범주형 : 카이제곱분석 - 분할표
# 범주형 / 연속형 : t-test(두집단), ANOVA(두집단 이상)
# 연속형 / 범주형 : logistic 회기분석, dicision tree
# 연속형 / 연속형 : 상관분석, 회기분석
str(airquality)
airquality$Ozone
(ta <- with(airquality, table(Ozone, Month)))
with(airquality,
      table(OzHi = Ozone > 80, Month, useNA = "always"))  # NA값도 파악해서

class( Titanic )
class( ta )
str( Titanic )
Titanic
ftable( Titanic, row.vars = 1:3) # 계층적으로 행 표현
ftable( Titanic, row.vars = 1:2, col.vars = "Survived")

# 카이제곱은 자유도에 따라 분포표가 변화 dchisq(df=1)
library(ggplot2)
ggplot(data.frame(x=c(0,10)), aes(x=x)) +
  stat_function(fun=dchisq, args=list(df=1), colour="black", size=1.2) +
  geom_text(x=0.6, y=1, label="df=1") +
  
  stat_function(fun=dchisq, args=list(df=2), colour="blue", size=1.2) +
  geom_text(x=0, y=0.55, label="df=2") +
  
  stat_function(fun=dchisq, args=list(df=3), colour="red", size=1.2) +
  geom_text(x=0.5, y=0.05, label="df=3") +
  ggtitle("Chisq-Distribution")

pchisq(q=2.5, df=2)   # 자유도가 2, 카이제곱값 2.5 => 누적확률
qchisq(p=0.8861537, df=1) # 분위수
# p:누적, d:확률값, q:분위수, r:random,
rchisq <- rchisq(n=100, df=2) # random
rchisq
hist(rchisq, breaks=20)

cleaner <- textConnection(
  "공기청정기 종류 관측도수
  1 7
  2 30
  3 16
  4 12
  5 15")
(tot = 7 + 30 + 12 + 15)
(x <- read.table(cleaner, header = T)) # T : true   F : false
# 자유도는 공기 청정기 종류 5 - 1 = 4
chisq.test(x$관측도수)  # 5% 유의 수준 p-value = 0.001042, 0.05 유의미
# 귀무 가설 : 기호가 같다.
# 대립 가설 : 기호가 같지 않다.
# 귀무 가설을 기각하고 대립 가설 채택
# 공기 청정기에 대한 기호가 다른 것으로 판단되다.
# 합리적인 이유를 설명하게 되면 보고서가 된다.

library(MASS)
data(survey)
str(survey)
xtabs(~ W.Hnd + Clap, data = survey)  # 필기하는 손, 박수칠 때의 손
class(xtabs(~ W.Hnd + Clap, data = survey)) # formula 사용하여 복잡한 경우
chisq.test(xtabs(~ W.Hnd + Clap, data = survey))
# p-value = 6.598e - 05 유의미
fisher.test(xtabs(~ W.Hnd + Clap, data = survey))
# 귀무가설을 기각하고 대립 가설 채택한다.

#비율검정 문제 
# 미국회사 직장인 400명과 한국회사 직장인 500명을 대상으로 조사를 한 결과, 
# 미국회사 직장인의 흡연율은 41%, 한국회사 직장인의 흡연율은 33%로 나타났다. 
# 그러면 미국회사와 한국회사 직장인의 흡연율(proportion of smokers)에는 차이가 
# 있다고 할 수 있는지 95% 신뢰구간에서 검정하시오.


#귀무가설 Ho : 미국회사와 한국회사의 흡연율은 차이가 없다 (p1 - p2 = 0)
#대립가설 H1 : 미국회사와 한국회사의 흡연율은 차이가 있다 (p1 - p2 != 0) 

prop <- c(0.41, 0.33)
n <- c(400, 500)
x <- prop * n
prop.test(x = x, # proprtion test
    n = n,
    alternative = c("two.sided"), # 양측 검정, 단측 검정
    conf.level = 0.95)            # 신뢰 구간
# p-vlaue = 0.0161 < 0.05
# 귀무 가설을 기각하고 대립 가설을 채택 => 흠연율에 차이가 있다.


# 정규분포 확률
# t 분포의 확률 값, 자유도에 영향을 받음
library(ggplot2)
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dt, args=list(df=3), colour="red", size=2) +
  stat_function(fun=dt, args=list(df=1), colour="yellow", size=3) +
  annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1) +
  annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2) + 
  annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3) +
  annotate("text", x=2.4, y=0.4, label="N(0,1)") +
  annotate("text", x=2.4, y=0.37, label="t(3)") +
  annotate("text", x=2.4, y=0.34, label="t(1)") +
  ggtitle("정규분포와 t분포")  

# shapiro.test 정규분포 여부를 테스트
# 귀무가설을 정규분포를 따른다.
# 대립가설은 정규분포를 따르지 않는다
# p-value = 0.4588 귀무가설 기가할 수 없기 때문에 정규분포
shapiro.test(rnorm(1000)) # 정규분포 1000개의 난수
set.seed(450)             # 컴퓨터의 난수는 의사 난수 => 정해진 난수, 순서를 바꿔
x <- runif(50, min = 2, max = 4)  # uniform : 균등 분포
shapiro.test(x)           # p-value = 0.003259 귀무 가설을 기각하고 대립 가설
# 결론 : 정규분포가 아니다.

# 일표본 t검정 (평균)
a = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
t.test(a, mu = 75)

# 그룹이 2개인 수면 관측 데이터
# 귀무가설 : 차이가 없다.
# 대립가설 : 차이가 있다.
str(sleep)
plot(extra ~ group, data = sleep)
t.test(extra ~ group, data = sleep)
# 표본 집단이 독립적인 경우
with( sleep, t.test( extra[group == 1], extra[group == 2] ) )
# 동일한 표본을 대상으로 하면 paired = TRUE  default: false
# 표본 집단이 동속적임
# 동일한 표본집단을 대상으로 했다.
# 귀무가설을 기각하고 대립가설 채택 - 그룹별로 수면 시간에 차이가 있다.
with( sleep, t.test( extra[group == 1], extra[group == 2], paired = TRUE ) )


# 등분산 테스트
# var.test
# 귀무가설 : 분산이 동일하다.
# 대립가설 : 분산이 동일하지 않다.
sleep
str(sleep2 <- sleep[, -3])
tapply(sleep2$extra, sleep2$group, mean)
var.test(extra~group, sleep)    # 귀무가설을 기각하지 못한다.
# 두 표본의 분산은 등분산이다.
t.test(extra~group, sleep, data=sleep2, paired=TRUE, var.equal=TRUE)


##
x1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
shapiro.test(x1)
shapiro.test(x2)  # 비모수
wilcox.test(x1, x2,
        alternative = c("greater"),   # 단측 검정 : 오른쪽에 대해서만 검증
        paired = TRUE,        # 동일 집단
        conf.level = 0.95)

# Error in xj[i] : invalid subscript type 'list'
wilcox.test(x1, x2,
        alternative = c("greater"),   # 단측 검정 : 오른쪽에 대해서만 검증
        paired = TRUE,        # 동일 집단
        exact = FALSE,        # 정확한 p값을 계산할 수 없다는 메세지가 나오면 넣어줌.
        conf.level = 0.95)
# 두 데이터의 평균차가 존재한다.

# MASS 패키지에 내장된 Cars93 데이터프레임의 가격(Price)과 생산국가(Origin) 
# 데이터에서 생산국이 (USA vs. non-USA) 2개의 group 에 대해서 차 가격(Price)의 
# 평균이 차이가 있는지를 검정해보시요.
# - 원산지별 가격차이가 있는가. 
# - 등분산성 테스트를 실시한다.  : 원산지 별 가격에 대한 등분산.
# - 유의수준은 0.05
library(MASS)
str(Cars93)
print(Cars93$Origin)
table(Cars93$Origin)
with(Cars93, tapply(Price, Origin, summary))  
boxplot(Price ~ Origin, data = Cars93,   main = "원산지별 가격",  xlab = "원산지",  
        ylab = "가격")

# 원산지별 정규분표인지 확인.
with(Cars93, tapply(Price, Origin, shapiro.test)) # 정규분포가 아니다.
var.test(Price - Origin, data = Cars93)            # 등분산 테스트 p-value = 0.01387
wilcox.test(Price ~ Origin, data = Cars93, alternative = c("two.sided"),
            var.equal = FALSE, exact = FALSE, conf.level = 0.95)
# p-value = 0.6724 귀무가설을 기각하지 못한다.
# 원산지별 가격차이가 존재하지 않는다. => 차이가 없다.

# anova : 2개 이상의 평균 검정
require(stats)
require(graphics)
attach(InsectSprays)
# 여섯개의 스프레이 ; 죽인 벌레 수 => 스프레이 별 성능차이가 있느냐
data(InsectSprays)
str(InsectSprays)
InsectSprays
xtabs(InsectSprays)   # 스프레이별 도수 => 합계
with(InsectSprays, mean(count[spray=="A"])) # A 스프레이에 대한 평균
with(InsectSprays, tapply(count, spray, mean))
with(InsectSprays, (tapply(count, spray, var))) # qnstks
with(InsectSprays, (tapply(count, spray, length)))
with(InsectSprays, (boxplot(count ~ spray)))

# 변수가 한개 인 경우
# 귀무가설 : 스프레이별로 기능차이가 없다.
# 대립가설 : 스프레이별로 기능차이가 있다.
with(InsectSprays, oneway.test(count ~ spray))  # p-value = 7.999e - 12
aov.out = aov(count ~ spray, data = InsectSprays)
summary(aov.out)    # Anova Test의 결과 확인
# 자유도 6-1, 차이제곱값의 합계, 차이 제곱값의 평균
# 분산비, 분산비에 대한 p-value 2e-16
# 스프레이는 기능 차이가 있다
TukeyHSD(aov.out)
summary.lm(aov.out)   # anova test나 회기분석이나 같은 방법
plot(aov.out)
# 잔차 (Residuals) = 에측치 - 실제 관측치
# 표준화 잔차 = 잔차( Residuals ) / 표준 편차 => 정규분포


#########################################################################
# 상관 분석:
# 분산 (var) : 평균으로 부터 데이터의 이격정도 : 크다 - 편차가 심하다
# 비 안정적이다
# cov(covariance) : 공분산  : 두 데이터간의 관게를 하나의 수치로 표현
# = sum((xi - x 평균)(yi - y평균) / (n - 1))
# cor(corelation) : 상관계수 : 공분산을 표준화
# 계수 : 표준화 -> 표준 편차
# 공분산행렬이나 상관 행렬은 정방행렬이면서 대칭행렬

1:5
cov(1:5, 2:6)   # 2.5 상관이 있다
cov(1:5, c(3, 3, 3, 3, 3))  # 상관이 없다.  독립변수간에는 상관이 없어야 함.
cov(1:5, 5:1)   # -2.5 역 상관이 있다

# 공분산 계산
a <- c(4.0, 4.2, 3.9, 4.3, 4.1)
b <- c(2.0, 2.1, 2.0, 2.1, 2.2)
c <- c(0.60, 0.59, 0.58, 0.62, 0.63)
(mat <- matrix(c(a, b, c), nrow=5, byrow=F))  # 동질적인 데이터를 사각형
(avr <- colMeans(mat))  # 열 평균
avr[1]
# 각 관측 데이터에서 평균을 공제
(acha <- a - avr[1])
(bcha <- b - avr[2])
(ccha <- c - avr[3])

(aa_var <- sum(acha * acha) / (length(a) - 1))
(ab_var <- sum(acha * bcha) / (length(a) - 1))  # 자유도
(bc_var <- sum(bcha * ccha) / (length(a) - 1))

cov(mat)

# 가로 세로의 열 이름이나 행 이름이 같다.
# 3X3 ; 정방행렬, 대칭행렬 : 대각선은 중심하고 대칭점의 상하값이 동일
# 정방 행렬이면서 대칭행렬인 행렬에 고유값 분해를 하면

# 행렬분해중 : 고유값 분해 -> 정방행렬만 가능, 비정방행렬 - 특이값 분해
# 특이값 분해 : 텍스트 마이닝 -> 문서의 종류(행), 단어(열) => 비정방
# 0이 많음 ==> 희소행렬 => 내적은 의미가 없음 => 특징

# 고유벡터와 고유값이 출력되는데 이 때
# 고유벡터는 정직교하는 정규화 벡터이다.
# 고유값은 각 축 방향으로의 크기를 나타낸다.
# 고유값 별로 정렬하면 주 성분이 결정 되고
# 그 주성분 중에 elbow 기법에 의해 선택된 변수가 noise를 제거한 주성분 변수이다.
# 이 선택된 주 성분으로 modeling을 하면 noise가 제거되어 accuracy가 높은
# 모델 생성이 가능하다.
# IOT 데이터는 노이즈를 포함하고 있어 반드시 주성분 분석후에 데이터를 분석해야 한다.
# 상광분석 귀무가설 : 상관이 없다.
#                 대립 가설은 상관이 있다.
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method="pearson")  # p-value = 0.02937
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method="spearman")
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method="kendall")


# IRIS
plot(iris$Sepal.Width, iris$Petal.Length)
cor(iris$Sepal.Width, iris$Petal.Length)

cor(iris[, 1:4])
plot(iris$Sepal.Length, iris$Petal.Length)
plot(iris$Petal.Width, iris$Petal.Length)

# 상관계수가 가장 높은 것은 : Petal.Length & Petal.width
# 상관계수가 가장 낮은 것은 : Sepal.width & Sepal.Length

d <- data.frame(x1=rnorm(10),
                x2=rnorm(10),
                x3=rnorm(10))
M <- cor(d) 
M
# install.packages("corrplot")
library('corrplot') 
corrplot(M, method = "circle") 
corrplot(M, method = "ellipse")
corrplot(M, method = "pie")
corrplot(M, method = "color")
corrplot(M, type = "upper")
corrplot(M, type = "lower")
corrplot(M, order = "hclust") # clustering 

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram = TRUE, pch = 19)

# Palette 색 파스첼
# 주성분 계수들 확인
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
M <- cor(mtcars[1:7])
heatmap(x = M, col = col, symm = TRUE)

library(dplyr)
(airquality)
str(airquality)
head(airquality)  #  Ozone 종속 변수.
(airquality_1 <- airquality[,c(1:4)])
airquality_1 <- select(airquality,c("Ozone", "Solar.R", "Wind", "Temp"))
(sum(is.na(airquality_1$Ozone)))
# NA는 계산결과가 NA는 데니터가 NA를 내포
cor(airquality_1)
# NA를 제거 : 행 제거 법 열 제거 법, 대체법(평균이나 0),
# 주변의 데이터를 고려 (rpart, knn) - 정밀함.
(airquality_2 <- na.omit(airquality_1))
(airquality_cor <- cor(airquality_2))
corrplot(airquality_cor, method="shade") 

## 회귀 분석
# 하나 이상의 독립 변수들이 종속변수에 미치는 영향을
# 추정할 수 있는 통계기법 -> 회귀선을 추정(절편, 기울기)
# 상관분석으로 정당성을 확보
# 회귀문석의 전제조건(잔차) : 등분산성, 독립성, 선형성, 정규성
# 비모수 검정 => 트리분석
y = c(1,5,3,8,5,3,10,7)     # 종속변수
x1 = c(2,4,5,6,8,10,11,13)  # 변수1
x2 = c(1,2,2,4,4,4,6,6)     # 변수2
opar=par(mfrow=c(1,3)) 
plot(x1,y)
plot(x2,y)
plot(x1,x2)
summary(lm(y~x1))       # 유의미하지 않고 Multiple R-squared:  0.3483,	Adjusted R-squared:  0.2396 
summary(lm(y~x2))       # Adjusted R-squared:  0.5846 
summary(lm(y~x1+x2))    # p-value: 0.002748, Adjusted R-squared:  0.8677 

# y = 1.0355 - 1.2223 * x1 + 3.6493 * x2 회귀식
y = 1.0355 - 1.2223 * 4 + 3.6493 * 2
y

y = 1.0355 - 1.2223 * 13 + 3.6493 * 6
y

# 실제 관측 값 - 예측값
(resi = 7 - y)

# 다음 데이터에서 키와 몸무게에 대하여 회귀분석을 하라.
str(women)
fit = lm(weight~height, data=women)
summary(fit)    # p-value: 1.091e-14 분석이 유의미함.
# Adjusted R-squared(수정된 결정 계수):  0.9903 
women.fitted_weight = -87.52 + 3.45 * women$height
plot(women$weight~women$height)
abline(fit, col="blue")
women$weight # 번주형 데이터가 아님 => 평가 : 상관계수
cor(women$weight, women$height)  # 0.9954948 => 상관 계수 1

#### 다음 데이터에 대하여 선형회귀를 실시하라
opar = par(mfrow = c(1, 1))
data(cars)
summary(cars)   # 일반함수를 오버라이딩해서 사용
plot(cars$speed, cars$dist)

# 회기 모델 생성
res.lm <- lm(dist ~ speed, data = cars)
summary(res.lm)
# 회기 분석 결과에 대한 계수 값, 피팅값, 잔차값, 분산값 확인 함수
coef(res.lm)  # coefficient
plot(cars$speed, cars$dist, xlab = "속도", ylab = "정지거리",
    xlim = c(0, 30), ylim = c(0, 125))
abline(coef(res.lm), col = 2)
fitted(res.lm) [1:4]    # 훈련 데이터에 대한 fitting 값
residuals(res.lm) [1:4] # 잔차값 확인
deviance(res.lm)

# 모델로 부터 새로운 데이터 예측
predict(res.lm, newdata=data.frame(speed=10))                         # 입력 데이터로 프레임 요구 : 21.74499
predict(res.lm, newdata=data.frame(speed=10), interval="confidence")  # 신뢰 구간 값
predict(res.lm, newdata=data.frame(speed=10), interval="prediction")  # 실제 오차
predict(res.lm, newdata=data.frame(speed=seq(4.0, 25.0, .21)), interval="confidence") 

## 다항 회귀
library(MASS)
data("Boston")
str(Boston) 
#  주택가격에 영향을 미치는 요소 분석 - 회귀 분석 
# crim  : 범죄율 , zn : 25,000 평방피트를 초과하는 거주지역 비율
# indus :비상업지역이 점유하고 있는 토지 비율  
# chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우)
# nox : 10ppm 당 농축 일산화질소 
# rm : 주택 1가구당 평균 방의 개수 
# age : 1940년 이전에 건축된 소유주택 비율 
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv : 소유 주택가격 중앙값 (단위 : $1,000) 

### R에서 가장 많이 나는 error는 Type 에러,
#   함수별로 parameter 타입과 return 타입이 다름.
head(Boston, 3)
boston_df <- as.data.frame(scale(Boston)) # z 점수 정규화 => 독립 변수가 종속변수 미치는
# 영향을 동일한 가중치 만들기 위해서
head(boston_df, 3)

set.seed(123)
# 데이터 선택
# 모델 참여한 데이터만 테스트 => 과적합 됨.
idx <- sample(1:nrow(boston_df), 300)  # random 함수를 사용하는 sample : index를 뽑음
trainDF <- boston_df[idx,]  # 훈련 => 훈련으로 만든 모델
testDF <- boston_df[-idx,]  # 테스트 => 결과가 잘 안나오면 => 과적합 된 상태
dim(trainDF)    # 300행 14열(변수) => 열 중심 언어
dim(testDF)     # 206행 14열
head(trainDF, 2)

# 종속 변수 ~ 독립 변수 (다항회귀)
form <- medv ~ crim + zn + indus + nox + rm + age + dis + rad + tax + ptratio + black + lstat
form2 <- medv ~ . # '.' 나머지 모두

lm_model <- lm(formula = form, data = trainDF)
lm_model
names(lm_model)

lm_model$fitted.values[1:5]   # 훈령 데이터(관측치)에 대한 예측
lm_model$residuals[1:5]       # 잔차
trainDF$medv[1:5]             # 실제 종속변수 값
# 잔차 != 예측된 값 - 실제값
lm_model$fitted.values[1] - trainDF$medv[1]
# 잔차 = 실제값 - 예측된 값
trainDF$medv[1] - lm_model$fitted.values[1]
lm_model$residuals[1]

summary(lm_model)
pred <- predict(lm_model, testDF)

cor(pred, testDF$medv)        # 평가 : 0.8425242


#########################################
insurance <- read.csv('./insurance.csv', header = T)
str(insurance)
dim(insurance)
insurance <- insurance[-6]
head(insurance)

install.packages("prettyR")
library(prettyR)
Mode(insurance$charges) 
summary(insurance$charges)  
hist(insurance$charges)     # 로그형태


cor(insurance[c('age', 'bmi', 'children', 'charges')])
install.packages("psych")
library(psych)
pairs.panels(insurance[c('age', 'bmi', 'children', 'charges')])

# 훈련 데이터와 테스트 데이터는 7:3
set.seed(123)
idx = sample(1:nrow(insurance), 0.7 * nrow(insurance))
training_ins = insurance[idx, ]
testing_ins = insurance[-idx, ]
(model_ins <- lm(changes ~ ., data = training_ins))
summary(model_ins)
y = -12045.33 + 269.39 * age - 22.02 * sample + 308.80 * bmi + 364.90 * children
summary(model_ins)    # 2.2e-16 모델이 유의미
    # 결적 계수 설명력 : Adjusted R-squared : 0.7427
(pred <- predict(model_ins, testing_ins)) # 테스트 데이터를 예측
cor(pred, testing_ins$charges)    # 평가 0.8719106

# 자기상관 : 이전 데이터가 다음 데이터에 미치는 영향이 있는가.
# 시계열 데이터에서 중요한 역할
# install.packages("lmtest")
library(lmtest)
# dwtest 귀무가설이 : 자기 상관성이 없다., 대립가설 : 자기 상관성이 있다.
dwtest(model_ins)

# 다중 공선성 확인
# install.packages("car")
library(car)    # 팽창 계수
sqrt(vif(model_ins)) > 2    # 다중 공선성 : 다중 공선성도 양호하다.


# 변수 선택법 : 전진선택법(forward), 후진 선택법(backward), 단계 선택법(both)
# 모델의 정확성을 높이기 위해서 변수를 추가 삭제
# 주성분 분석(PCA), 변수 중요도(트리분석), 변수 선택법
step(model_ins, direction = "both")
model_ins <- lm(charges ~ age + bmi + children + smokers, data = training_ins)
model_ins
summary(model_ins)

# Domain Knowledge 경험
# 파생 변수 추가 ( 고차항 => age ** 2), I : i = inhibit
model_ins2 <- lm(charges ~ age + I(age ** 2) + children + bmi + smokers, data = training_ins)
model_ins2

##### tree
# install.packages("rpart")
library(rpart)

result <- sample(1 : nrow(iris), nrow(iris) * 0.7)

train <- iris[result,] 
test <- iris[-result,] 
dim(train); dim(test)

formula <- Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width

model <- rpart(formula = formula, data = train)
model

pred <- predict(model, test)
cpred <- ifelse(pred[,1] >= 0.5, "setosa", 
                ifelse(pred[,2] >= 0.5, "versicolor", "virginica"))
tr<-table(cpred, test$Species)
(20+13+10) / nrow(test)
paste(round(sum(diag(tr)) / nrow(test)*100), '%')
plot(model)
text(model, use.n=T, cex=0.6) 
post(model,use.n = TRUE, file="")

# install.packages("rpart.plot")
library(rpart.plot)

result <- sample(1: nrow(iris), nrow(iris) * 0.7)

train <- iris[result,]
test <- iris[-result,]

dim(train)  # 105행 5열
dim(test)

# 분류
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

model <- rpart(formula = formula, data = train)
model

pred <- predict(model, test)  # 종의 범주별 확률 값
# setosa versicolor virginica
cpred <- ifelse(pred[, 1] >= 0.5, "setosa",   # 3항 연산자
                ifelse(pred[, 2] >= 0.5, "versicolor", "virginica"))

# 평가
tr <- table(cpred, test$Species) # 테이블
(20 + 13 + 10) / nrow(test)
# 트리분석의 단점 : 할때마다 값이 달라질 수도 있고, 과적합
# 가지치기 -> 과적합 방지
# 트리분석의 장점 : white box 데이터 내부 확인 가능, 변수 중요도
# 특징 추출 : PCA, FA, 변수선택법, 
(11 + 19 + 12) / nrow(test)   # 0.9333333

# 평가 행렬 - 정방 행렬
# 대각값을 취하는 함수 : diag nal (대각의)
paste(round(sum(diag(tr)) / nrow(test) * 100), '%')   # paste는 문자열 결합함수
plot(model)
test(model, use.n = T, cex = 0.6)
post(model, use.n = T, file = "")

install.packages("rpart.plot")
library(rpart.plot)

install.packages("rattle")
library("rattel")

prp(model)
rpart.plot(model)
fancyRpartPlot(model)

# 정확도를 높이는 방법 kagle => 결과로만 판단 => 정보기반 학습
# DT -> RF(Random Forest) -> Ada Boost -> Gradient Boost -> XGBoost (단독)
# 100% 분류 => stacking (여러개의 모델을 생성 -> 다시 결과 훈련(분류 : 추첨))
# 정확한 모델 3개가 아니라 결과 상관성이 없는 모델 3개 선택

### 트리분석  : 정보기반 -> 불순도 => 데이터를 분할 => 복잡
#   불순도 척도 
#   카이제곱 통계량, 지니지수, 엔트로피 지수
#   party 패키지의 ctree는 입력 변수 레벨이 31개로 제한되어 있음.
#   스스로 가지치기를 함.
#   범주형 데이터를 제외하고, 선형 분석을 이용해서 분류
#   rpart : CART : Classification And Regression Tree 를 구현
#   gini index = 불순도 척도로 지니치수를 쓰고 가지치기로 과적합을 방지함.
library(party)
result <- sample(1 : nrow(iris), nrow(iris) * 0.7) 
table(result) 
train <- iris[result,] 
test <- iris[-result,]  
head(train)
formula <- Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width     
# 범주형으로 출력 
iris_ctree <- ctree(formula, data=train) # 학습데이터로 분류모델(tree) 생성
iris_ctree
plot(iris_ctree, type="simple") 
plot(iris_ctree)
pred <- predict(iris_ctree, test) # 예측치 
pred
tr <- table(pred, test$Species)
paste( round( sum( diag( tr ) ) / nrow(test) * 100) , '%')


# 암 진단 유무(diagnosis)를 결정
library(rpart) 
library(rpart.plot)
# install.packages("rattle")
library('rattle')
# R은 데이터 로딩시에 텍스트를 factor화 하는 경향
wdbc <- read.csv('./wdbc_data.csv', stringsAsFactors = FALSE)
str(wdbc)
wdbc <- wdbc[-1]
head(wdbc)
head(wdbc[, c('diagnosis')], 10)    # 종속 변수 : text
# 점주화
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))  #  양성, 악성
# NA 처리, 이상치 처리, 정규화
wdbc$diagnosis[1:10]
normalize <- function(x){           # min-max정규화 , scale정규화 (정규분포에서 확률)
  return ((x - min(x)) / (max(x) - min(x)))   # 관측치 / 범위값  0 ~ 1 사이의 값으로 정규화
}

wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))    # lapply의 결과값은 list
wdbc_x
summary(wdbc_x)
# 정규화한 데이터와 범부형 종속변수를 묶음
wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)
idx = sample(nrow(wdbc_df), 0.7*nrow(wdbc_df))
wdbc_train = wdbc_df[idx, ] 
wdbc_test = wdbc_df[-idx, ]
model2 <- rpart(wdbc.diagnosis ~ ., data = wdbc_train)
pred2 <- predict(model2, wdbc_test, type = 'class')
fancyRpartPlot(model2)
# 대각선 값 :  정분류 율, 오분류율

# 예측값, 실제값
# 실제 양성 => 음성으로 예측 => 과잉 진료 => 민감도, 재현율 recall
# 실제 악성 => 양성으로 예측 => 사망 => 특이값
# precision : 
(tr <- table(pred2, wdbc_test$wdbc.diagnosis))

# 정분류율
paste (round(sum(diag(tr)) / nrow(wdbc_test)*100), '%') 
# 오분류율
paste(    round(sum(tr[1,2], tr[2,1])/ nrow(wdbc_test) *1000)/10, '%')
# precision rate (정밀도)
paste (round( tr[1,1] / sum(tr[1,]) *100), '%') 
# sinsitivity rate (민감도) # 1-민감도: 암이 아닌데 암으로 판명(과진료)
paste (round( tr[1,1] / sum(tr[,1]) *100), '%') 
# specificity rate (특이도) # 1-특이도 : 암인데 암이 아닌 것으로 판정 (큰병)
paste (round( tr[2,2] / sum(tr[,2]) *100), '%')  
 

predicted <-c(1,0,0,1,1,1,0,0,0,1,1,1)
actual   <- c(1,0,0,1,1,0,1,1,0,1,1,1)
results<-xtabs( ~  predicted + actual  )
results
sum(predicted==actual) / NROW(actual)  

# install.packages("caret")
library(caret)
# 자동화 패키지, parameter tunning, 모든 모델을 하나의 인테이스 처리

# install.packages("e1071")
library(e1071)

predicted_f <- factor(perdicted)
actual_f <- factor(actual)
confudionMatrix(predict_f, actual_f)
# 아무 정보가 없더라도 NoInformation Rate : 0.6667
# 여러사람이 모델을 생성 : 모델을 평가
# 우연히 맞출 수 있는 경우의 수를 제외하고 모델이 맞힌 것






