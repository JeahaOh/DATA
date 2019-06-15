# Matrix
(a = matrix(1:9, nrow = 3, ncol = 3))   # 3행 3열로 matrix를 만든다.

matrix(1:9, nrow = 3)
class(a)
attributes(a)
dim(a)


(a = matrix(1:9, nrow = 3, ncol = 3, byrow=T)) # 행 우선
matrix(1:12, nrow = 3)
class(a)
attributes(a)
dim(a)  # 속성을 확인하는 함수

colnames(a) <- c("C1", "C2", "C3")  # 열 이름을 지어줌  [1,1] [1,2] [1,3]
rownames(a) <- c("R1", "R2", "R3")  # 행 이름을 지어줌  [2,1] [2,2] [2,3]
a
# matrix는 행과 열로 만들어 져 있어서, 행과 열로 접근할 수 있다.
y <- matrix(nrow=2, ncol=2) # NA로 구성된 matrix
y[1,2]
y[1,1] <- 1
y[2,1] <- 2
y[1,2] <- 3
y[2,2] <- 4
y


(x = matrix(1:9, nrow=3, byrow=T))
(y = matrix(11:19, nrow=3, byrow=T))
(c=cbind(x,y)) # column : 열
(r=rbind(x,y)) # row : 행
# 옹?

t(x)  # 전치행렬 : 행과 열을 바꿈.
# 행렬 : 행과 열
# 벡터 * 행렬 = Vector를 반환(MRS : move, rotate, scale)
(x = matrix(1:12, nrow = 3, ncol = 4))
x %*% x  # 안 곱해지는 이유는 행렬 X 행렬은 앞의 행렬의 열수와 뒤의 행렬의 행수가 일치해야 하기 때문임.
x * x  # 같은 위치
x %*% t(x)  # 행렬 곱.

#  V = c(1,2)
#  Cosø - Sinø
#  Sinø Cosø
#  행과 열을 뒤집음. 뭐라는거야?
#  행렬의 곱은 변환의 합...

# 유사도 : 벡터 * 벡터 내적을 했을때 유사한 값을 낼 수 있다.


nrow(x)
ncol(x)
dim(x)

rowMeans(x)  # 행 평균
rowSums(x)   # 행 합계
colMeans(x)  # 열 평균
colSums(x)   # 열 합계

diag(x)      # 대각 행렬

# 상관 행렬, 거리값 행렬( 자동 정방 행렬)
(x = matrix(1:16, nrow = 4, ncol = 4, byrow=T))
# 고유값 분해는 정방 행렬만 가능.
# 정방 행렬이 아니면 특이 행렬 분해( 연관 분석, text mining)
eigen(x)     # 고유값 분해 = 고유치(크기값) + 고유 벡터(방향 값)
# 상관 행렬을 고유값 분해 고유치와 고유 벡터.
# 주성분분석(Noise 제거)의 결과값 = 학습을 정확하게 하도록 특징 추출.
#  고유 벡터는 정직교 하는 벡터, 축이 90도 이루어진, 정 직교.
#  두 벡터의 내적이 0이면 직교한다.

## !!왜 쓰는지만 알면 됨!!
## 이미 누가 다 만들어 놨으니까 어떻게 돌아가는지만 알면 됨


x * 3
# 연립 방정식의 해
(x = c(1, 3, 5, 2, 5, 1, 2, 3, 8))
(mat = matrix(x, nrow=3, ncol=3, byrow=T))
(b = c(10, 8, 3))
solve( mat, b )
dim(x)
# max의 주소 값 전달.
# 데이터를 입력 -> 처리 -> 결과값
xy = c(2,3,5,2,5,1,2,3,8)
(x = c(1, 3, 5, 2, 5, 1, 2, 3, 8, 10))
(mat=matrix(x,nrow=3,ncol=3, byrow=T))
dim(mat)
apply(x, 1, max)  # 행방향 : 함수를 적용 max( 1 2 3 )
# max(4 5 6), max(7 8 9)
apply(x, 1, min)  # 
apply(x, 1, sum)

## 안되는데 설명 제대로 안해주고 넘어감.



# list ( size가 달라도 됨. 사각형을 유지하지 않아도 됨. )
#     : 함수의 매개변수나 리턴 값으로 사용됨.
x <- c(82, 95, 78)  # 3개.
y <- c(84, 92)  # 2개.
z <- c(96, 86, 82, 77) # 4개.
(core <- list(수학 = x, 영어 = y, 국어 = z))
core
typeof(x["수학"])
typeof(core["수학"])
typeof(core[["수학"]])
core$"수학"


# data.frame
## 입출력의 기본 포멧, 사각형을 유지,
## 한 열 내 동질적, 열간 이질적.
x <- c(10, 20, 30, 40)
x <- c(6, 7, 8, 9)
data <- data.frame(길이 = x, 높이= y)  # 열 이름, 데이터 분석 변수.
data
str(data)  # 데이터의 구조만 보여줌. 데이터가 많으면 다 안보여주고 몇개만 보여줌.
data$길이
data[,1]  # 1번 열의 데이터를 보여달라.
head(data[c("길이", "높이")])  # 상위 6개
head(data[c("길이", "높이")], 2)  # 개수 제한 2개
data$둘레 = c(30, 40, 50, 60)  # 실시간으로 데이터 추가. (갯수를 지켜줘야 함.)
data


# tree
str(trees)
head(trees, n = 3)
tail(trees, n = 3)
trees[2 : 3,]
trees[trees$Height > 82,]
trees[10 : 12, 2]
trees[10 : 12, 2, drop = F]

summary(trees) # 변수별로 Min, Max, 1사 분위 수, 증위수, 평균
boxplot(trees) # 중위수 : 이상치에 영향을 덜 받기 때문에 통계에서는 중요함.


# 제어문
x <- 0
if( x < 0 ) {
  print("Negative No")
} else if ( x > 0 ) {
  print("Positive No")
} else 
  print("Zero")

# 반복문 ## R이나 Python은 for in 문을 많이 쓴다.
i <- (1:10)
for( n in i ){
  print(n)
}
i = 0
while( i < 10 ) {
  i <- i + i
  print(i)
}

# 두수분포표 / 누적 도수 분포표
coin <- c(2, 2, 0, 1, 1, 1, 2, 3, 1, 2, 2, 3, 23, 4, 1, 3, 2, 3, 3, 1,2,2,1,2,2)
#  종류별 카운트
(coin.freq <- table(coin)) # 도수 분포 표
# 산포도 출력
# x축 y축
plot(0:4, coin.freq, main="4회 동전 던지기 결과", xlab="앞면이 나온 횟수",
     ylab="빈도수", ylim = c(1, 30), type="b") # both, l:line, p: point
(cumfreq = cumsum(coin.freq))
plot(1:4, cumfreq, main="4회 동전 던지기 결과", xlab="앞면이 나온 횟수", 
     ylab = "빈도수", ylim= c(1, 30), type="1")
##?

## 함수 : 컴퓨터 프로그래밍의 방법으로 stack(함수가 실행될 때 필요한 데이터는 stack에 존재)
# 포인터
# 매개변수 : 함수에 전달 되는 입력 값
  # 필요 매개변수
  # 디폴트 매개변수
  # 키이 매개변수
pow = function ( x, y = 2){ # 함수는 - 메모리 주소 값
  result <- x ^ y
  print(paste(x,"의 ", y, "승은 ", result))
}
pow(8)
pow(8,2)
pow(x = 8, y = 2)
pow(y=2, x=8)
pow(x=8, 2)
pow(2, x=8)



# 정규분포
rnorm(sd = 5, mean = 1, 100) # random normal distribution(정규 분포) : 평균이 0
sd(rnorm(sd = 5, mean = 1, 100))
data <- c(1:10, rnorm(10, 2), runif(10)) # uniform 균등 분포
data

groups <- gl(3, 10) # generate level : 팩터형 데이터 생성
groups
# tapply 그룹핑
tapply( data, groups, print )  # 그룹별로 출력
tapply( data, groups, mean )  # 그룹별로 평균 내기
list(rep(1,4), rep(2,3), rep(3,2), rep(4,1))
mapply(rep, 1:4, 4:1) # multiple apply

str(mtcars)  # mtcars 미리 넣어둔 예제 데이터.
(exdata <- mtcars[1:10, 1:3])
tapply(exdata$mpg, exdata$cyl, mean) # 기통별로 연비의 평균

install.packages("plyr")
library(plyr)

x= data.frame(ID = c(1,2,3,4,5), height = c(160, 171, 173, 162, 165))
y= data.frame(ID = c(6, 4, 1, 3, 2), weight = c(50, 73, 60, 57, 80))

(z <- join(x,y,by='ID')) # left join // key 가 같은 데이터를 묶음
(z <- join(x,y,by='ID', type="inner"))  # 대응 하는 것들만 나옴.
(z <- join(x,y,by='ID', type="full"))   # 대응 하지 않아도 나옴.


data(iris)  # iris 꽃에 대한 데이터
str(iris)
tapply(iris$Sepal.Length, iris$Species, mean)
# 입력 data.frame, 출력 data.frame
# 변수
# 요약
(a <- ddply(iris, .(Species), summarise, avg = mean(Sepal.Length)))

# 사용자 정의 함수
# 함수 y = ax => 기울기(slope), 절편(intercept)
irisffects <- ddply(iris, .(Species), # coef 계수를 출력, lm(선형 회귀)
                    # 독립 변수(x), 종속 변수(y) # coefficient 계수를 출력.
                    function(df) coef(lm(Sepal.Width~Sepal.Length, data=df)))
irisffects

# base 전처리 하수
data(airquality)
str(airquality)
airquality$ozone
# 파생 변수
## 데이터를 적당히 변경해줘야 유의미한 데이터가 됨.
aq <- within(airquality, { # with(값 변경 불가), within(값 변경 가능) 데이터 프레임 이름 생략 가능.
  lOzone <- log(Ozone)  # 정규 분포
  Month <- factor(month.abb[Month]) # 번주화
  cTemp <- round((Temp - 32) * 5 / 9, 1) # 화씨를 섭씨로 바꿈.
  S.cT <- Solar.R / cTemp # 데이터 가공 볕/기온
  rm(Day, Temp) # 날짜와 온도는 볼 필요가 없으니 지움.
})
head(aq)


# 데이터를 분리
a = (split(iris, iris$Species)) # split list를 반환함.
a
class(a) # list
a$versicolor
class(a$versicolor)
a$versicolor$Sepal.Length


search()  # 메모리에 올려진 패키지 들을 찾음.
attach(airquality) # 데이터 덩어리를 패키지 처럼 사용 할 수 있음.
transform(Ozone, logOzone = log(Ozone))
search()
detach(airquality)
search()
ls()

# 일부 데이터를 필터링 할 때 사용하는 subset
subset(iris, subset = (Species == "setosa")) # 필터링
subset(iris, subset = (Species == "setosa" & Sepal.Length > 5.0))
subset(iris, select = c(Sepal.Length, Species)) # 열 선택
subset(iris, select = -c(Sepal.Length, Species))

names(iris)
iris[, !names(iris) %in% c("Sepal.Length", "Species")]


str(mtcars)
attach(mtcars)
## mtcars
#  실린더가 4 ~ 6개인 자동차들 중 연비, 실린더수, 변속기 종류를 출력.
subset( mtcars, select = c( mpg, cyl, am), subset = (cyl %in% c(4, 6)))
#  자동변속기인 자동차를 선택.
subset( mtcars, select = c(mpg, cyl, am), subset = (am == 0))
#  연비(mpg)가 20보다 큰 자동차를 선택.
subset( mtcars, select = c( mpg, cyl, am), subset = (mpg > 20))


IloveR = paste("I", "love", "R", sep = "-") # 분리자 (seperator)
no_quotes = noquote(c("some", "quoted"))
class(no_quotes)
is.character(no_quotes)
typeof(no_quotes)

format(c(6, 13.1), digits = 2)
format(c("A", "BB", "CCC"), width = 5, justify = "centre") # 자리수 확보 정렬
nchar("How")
tolower(c("President"))
toupper(c("President"))
substring("ASDF1020", 2, 4)

# 검색 : grep
head(USArrests)
(states = rownames(USArrests))
grep(pattern = "K", x = states)
grep(pattern = "K", x = states, value = T)
grep(pattern = "[wW]", x = states, value = T) # 정규 표현식 [] 선택 


money = "$money"
sub(pattern = "$", replacement = "", x = money)  # sub = substitute
sub(pattern = "\$", replacement = "", x = money)
sub(pattern = "\\$", replacement = "", x = money)

sub("\\d", "_", "the dandelion war 2010") # 
gsub("\\d", "_", "the dandelion war 2010") # global

install.packages("stringr")
library(stringr)
str_length(c("i", "like", "programming", NA))  # 문자열들의 길이를 구해줌.
str_c("May", "The") # 문자열들을 합쳐줌.
str_sub("adios", 1:3)
str_extract("asdf1020asdf33", "[1-9]{2}")  # [] 선택 {} 반복 갯수
str_extract_all("asdf1020asdf33", "\\d")
str_extract_all("asdf1020asdf33", "\\S")

fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "_")
str_replace_all(fruits, "[aeiou]", "_")

strings <- c(" 219 733 8965", "329-293-8753", "banana", "595 794 7569")
phone <- "([2-9][0-9]{2})[- .]([0-9{3})[- .]([0-9]{4})"
str_extract(strings, phone)
str_extract_all(strings, phone)


shoppinglist <- c("2017-10-19 수입 3000원", "2017-10-20 수입 4500원", "2017-10-21 수입 2500원")
# 1) 일자별 수입중에서 다음과 같은 형태로 출력.
# 결과) "3000원" "4500원" "2500원"
str_extract_all( shoppinglist, "[0-9]{4}원", simplify = T)
str_extract_all( shoppinglist, "\\d{4}원", simplify = T)
(shoppinglist2 <- str_split(shoppinglist, "", simplify = T))
(shoppinglist3 <- shoppinglist2[, 2])
str_replace_all(shoppinglist,"-", "/")
# 위 데이터중 날짜만 추출
str_extract_all( shoppinglist, "[0-9]{4}[-][0-9]{2}[-][0-9]{2}", simplify = T)
str_extract_all( shoppinglist, "수입 [0-9]{4}원")

# USArrest의 주이름 마다 포함된 모음의 개수를 그래흐포 변환 출력하기.
head(USArrests)
(states = rownames(USArrests))

str_count(states, "a")
str_count(tolower(states), "a")

vowels = c("a", "e", "i", "o", "u")
(num_vowels = vector(mode = "integer", length = 5))
for(j in seq_along(vowels)) {
  num_aux = str_count(tolower(states), vowels[j])
  num_vowels[j] = sum(num_aux)
}
names(num_vowels) = vowels
num_vowels

sort(num_vowels, decreasing = T)
barplot(num_vowels, main = "주이름에 포함된 모음의 개수", 
        border = NA, ylim = c(0, 80))


## 완전히 이해 할 필요는 없고 이런게 있다는것만 알면 써먹을 수 있다고는 하는데...