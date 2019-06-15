# R 대입문.
# R 갑의 대입과 변수의 대입.
# 메모리 공간 : 주소.
# R은 모든 데이터를 가리키는 변수에는 주소값이 들어있다.
# 값이 직접 들어 있지 않다.
# 중요한 이유, 함수에 데이터를 전달하는 방식이 주소에 의해 전달되어 진다.
a <- 10   # stack에는 변수, 값은 heap에 저장.
b <- a    # 값이 변경 되려면, 대입에 의해 변경된다.
b
print(b)
( a <- 20 )  # 대입을 하면서 값을 확인하려면 괄호를 쳐줌.
print(a)
b

# <- 대입.

# 컴퓨터는 0으로 나누면 안된다.
1 / 0    # Inf
0 / 0    # NaN
Inf + NA   # NA 데이터에 결측치가 있으면 안됨.
# 데이터 타입 확인하기.
class(a)
typeof(a)

# 가금승제(연산).
(x <- 5)
(y <- 16)
(x + y)
(x - y)
(x * y)
(x / y)
(x %/% y)   # %/% 몫 연산자.
y%%x        # 나머지 연산자.
y^x         # 지수
2^3

# 관계 연산자. (결과 TRUE / FALSE 대문자)
x < y
x > y
x <= 5
y >= 20

# 수식.
num <- 6
# 연산의 순서를 바꾸는데 괄호를 사용함.
num <- (num ^ 2 * 5) + (10 / 3)
num

# 내가 갖고 있는 변수들을 보는 명령어.
ls()
# 변수들 지우기 : 가능한환 메모리를 확보해야 큰 데이터를 사용할수 있음. 그래서 데이터를 정리해줌.
rm(list = ls())


## 데이터 타입 확인
# R은 데이터 타입을 지정하지 않음 : 개발자가 홗인.
int <- 50
string <- 'Jeaha'
boolean <- TRUE
is.numeric(int)
is.character(string)
!is.logical(boolean)


# vector : 값 대입시 Combine의 c
score <- c(85, 95, 75, 65)
score
mean(score)
sum(score)
var(score)    # 분산.
sd(score)     # 표준 편차.


score <- c(85, 95, NA, 75, 65)
score
mean(score)
mean(score, na.rm = TRUE)
?mean        # Help 창에 API 가 뜸 ㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋ
# 함수 인자에 '=' 이 없는것은 필수 매개변수,
# '='이 있는것은 선택 매개변수.

# 컴퓨터에 데이터를 입력 => 10으로 저장됨 : 문자와 숫자를 구별하기 위해 쿼티션을 붙임.
# A => 65  # ASCII code표로 저장됨.
x <- c(10, 20, 30, '40')  # 문자 '',""
mode(x)
class(x)    # vector안의 데이터는 동질적이기 때문에, 앞에 숫자들은 자동으로 문자로 변환함. 미친놈.
(xx <- as.numeric(x))
sum(x)
sum(xx)
class(xx)


# factor  # R에서의 index는 1부터 시작함.
gender <- c('M', 'F', 'M', 'F', 'M')  # 문자 벡터.
gender

plot(gender)   # 산포도를출력 시도. 문자라 안됨.
fgender <- as.factor(gender) # 문자로도 범주형.
fgender
fgender[2]  # index
mode(fgender); class(fgender)  # gender를 factor로 변환하면 숫자로도, 문자로도 취급 할 수 있다. / factor는 문자로도 숫자로도 쓸 수 있다.
levels(fgender)
plot(fgender)



# date : 입력을 문자열
today <- '2019-06-15 11:56:64'
class(today)
today2 <- as.Date(today)  # 입력은 문자 아니면 숫자로 받기 때문엔 먼저 문자로 입력해준 뒤 Date로 변환 해 준다.
today2
class(today2)

# Default DATA
data()
Nile # 나일강의 강수량
length(Nile)
plot(Nile)

## 공부하기 쉽게, 샘플 데이터가 있음.
## R의 샘플 데이터로 공부하고 Python으로 넘어가라.


# vector(배열로 저장)
# 선형 대수로 문제를 해결하기 때문에
# 내적 : 사이각과 같은 의미로 사용함.

a <- c('apple', 'orange', 'banana')
length(a)
NROW(a)  # data의 갯수를 확인할 수 있음.
(x=vector("list", 10))

(x <- 1:7)   # 번위 연산자 : colon
(y <- 2:2)
(y <- 2:-2)

seq(1, 3.2, by=0.2) # esquence by : 증분
seq(1, 5, length.out=4)  # 결과가 4개

(d <- rep(1:4, 2))
(d <- rep(1:3, each=3))  # 각 각.
(d <- rep(1:3, 2, each=3))

# 인덱싱
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x
x[3]
x[c(2, 4)]  # indexing을 vector로...
x[-1]     # 1번째를 제외 하고
x[c(2, -4)]  # 에러... 인덱스를 +/-혼용 할 수 없음.
x[c(2.4, 3.54)] # 정수만 가능.
x[c(TRUE, FALSE, FALSE, TRUE)]  # BOOLEAN INDEXING : recycling true인 인덱스의 값만 가져옴.
x[x < 3]  # BOOLEAN INDEXING : 관계 연산자를 이용, 3 이하만 가져옴.
x[x > 3]  # 3 이상만 가져옴.


# vector 연산 (하나씩 계산하는 개념이 아님.)
x = c(3, 6, 8)
y = c(2, 9, 0)
x + y # 요소끼리 더함.
x + 1
x + c(1, 4)  # 숫자의 개수가 안맞으면 연산이 안됨. 배수로 하면 가능.
x + c(1, 4, 0)
x + c(1, 1, 1, 2, 2, 2)

"a" %in% c('a', 'b', 'c')
x <- 1:5
all(x > 5)
any(x > 5)

# 정렬
x <- c(3,2,6,4,5,8,1)
x
sort(x)  # 오름차순.
sort(x, decreasing = T) # 내림차순
x  # 원본은 그대로 있음.

order(x) # 오름차순 순서
order(x, decreasing = T)
x[order(x)]
# order 어디에 쓰는거임?

