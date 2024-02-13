library(tidyverse)

#총 출전 시간이 많으면 자연스럽게 각 스탯의 총 횟수는 많아질 것. 따라서 이를 고려한 salary와의 관계와 실력적인 면, 즉 각 스탯의 총 횟수들을 게임 출전 횟수로 나누어 한 게임당 평균으로 salary와의 관계를 비교하여 살펴 볼 것. 또한 포지션 별로 각 스탯이 상이하게 나타날테니 이 또한 고려하여 살펴 볼 것.

### 1.변수별 분포 확인
stat=read.csv('stat.csv',header=TRUE)
str(stat)
#### 1-1) 총 출전 시간, 평균 출전 시간

##### 총 출전 시간
stat %>% 
  ggplot(aes(MIN))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 100)+
  labs(title='총 출전 시간 분포',
       y = "Count", 
       x = "총 출전 시간")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(MIN),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 출전 시간 분포',
       y = "Count", 
       x = "총 출전 시간")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(MIN),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 출전 시간 분포',
       y = "Count", 
       x = "총 출전 시간")

#특별한 특징은 보이지 않는다

##### 평균 출전 시간
stat %>% 
  ggplot(aes(MIN.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 1)+
  labs(title='평균 출전 시간 분포',
       y = "Count", 
       x = "평균 출전 시간")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(MIN.),
                 color='white',
                 position='fill',
                 binwidth = 1,
                 alpha=.7)+
  labs(title='포지션 별 평균 출전 시간 분포',
       y = "Count", 
       x = "평균 출전 시간")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(MIN.),
                 color='white',
                 position='stack',
                 binwidth = 1,
                 alpha=.7)+
  labs(title='포지션 별 평균 출전 시간 분포',
       y = "Count", 
       x = "평균 출전 시간")

#특별한 특징은 보이지 않는다

##### 포지션 별 boxplot(총 출전 시간, 평균 출전 시간)
##### 총 출전 시간
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(MIN)), 
             y=MIN, 
             fill=fct_reorder(Pos, desc(MIN))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 출전 시간 분포',
       y='총 출전 시간',
       x='포지션',
       fill='포지션')

#큰 차이가 있지는 않으나 파워포워드의 경우 조금 낮은 징후를 보인다

##### 평균 출전 시간
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(MIN.)), 
             y = MIN., 
             fill = fct_reorder(Pos, desc(MIN.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 출전 시간 분포',
       y='평균 출전 시간',
       x='포지션',
       fill='포지션')

#센터나 포워드가 평균 출전 시간이 비교적 낮은 모습을 보인다

#### 1-2)총 골 넣은 점수, 평균 골 넣은 점수
##### 총 골 넣은 점수
stat %>% 
  ggplot(aes(PTS))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 100)+
  labs(title='총 골 넣은 점수 분포',
       y = "Count", 
       x = "총 골 넣은 점수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PTS),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 골 넣은 점수 분포',
       y = "Count", 
       x = "총 골 넣은 점수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PTS),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 골 넣은 점수 분포',
       y = "Count", 
       x = "총 골 넣은 점수")

#센터의 경우 총 골 넣은 점수가 매우 높은 경우에 분포하지 않는다

##### 평균 골 넣은 점수
stat %>% 
  ggplot(aes(PTS.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 1)+
  labs(title='평균 골 넣은 점수 분포',
       y = "Count", 
       x = "평균 골 넣은 점수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PTS.),
                 color='white',
                 position='fill',
                 binwidth = 1,
                 alpha=.7)+
  labs(title='포지션 별 평균 골 넣은 점수 분포',
       y = "Count", 
       x = "평균 골 넣은 점수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PTS.),
                 color='white',
                 position='stack',
                 binwidth = 1)+
  labs(title='포지션 별 평균 골 넣은 점수 분포',
       y = "Count", 
       x = "평균 골 넣은 점수")


#특별한 특징은 보이지 않는다


##### 포지션 별 boxplot(총 골 넣은 점수, 평균 골 넣은 점수)
##### 총 골 넣은 점수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(PTS)), 
             y=PTS, 
             fill=fct_reorder(Pos, desc(PTS))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 골 넣은 점수 분포',
       y='총 골 넣은 점수',
       x='포지션',
       fill='포지션')

#포지션 별로 큰 차이는 보이지 않으나 슈팅가드의 경우 매우 높은 점수들이 분포한다.


##### 평균 골 넣은 점수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(PTS.)), 
             y = PTS., 
             fill = fct_reorder(Pos, desc(PTS.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 골 넣은 점수 분포',
       y='평균 골 넣은 점수',
       x='포지션',
       fill='포지션')

#포지션별로 큰 차이를 보이지는 않는다



#### 1-3) 총 2점 슛 성공 횟수,시도 횟수 및 성공 확률

##### 총 2점 슛 성공 횟수
stat %>% 
  ggplot(aes(FGM))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 10)+
  labs(title='총 2점 슛 성공 횟수 분포',
       y = "Count", 
       x = "총 2점 슛 성공 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FGM),
                 color='white',
                 position='fill',
                 binwidth = 50,
                 alpha=.7)+
  labs(title='포지션 별 총 2점 슛 성공 횟수 분포',
       y = "Count", 
       x = "총 2점 슛 성공 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FGM),
                 color='white',
                 position='stack',
                 binwidth = 50,
                 alpha=.7)+
  labs(title='포지션 별 총 2점 슛 성공 횟수 분포',
       y = "Count", 
       x = "총 2점 슛 성공 횟수")

#큰 차이는 보이지 않는다

##### 총 2점 슛 시도 횟수
stat %>% 
  ggplot(aes(FGA))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 20)+
  labs(title='총 2점 슛 시도 횟수 분포',
       y = "Count", 
       x = "총 2점 슛 시도 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FGA),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 2점 슛 시도 횟수 분포',
       y = "Count", 
       x = "총 2점 슛 시도 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FGA),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 2점 슛 시도 횟수 분포',
       y = "Count", 
       x = "총 2점 슛 시도 횟수")

#큰 특징은 나타나지 않는다


##### 2점슛 성공 확률
stat %>% 
  ggplot(aes(FG.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 1)+
  labs(title='2점 슛 성공확률 분포',
       y = "Count", 
       x = "2점 슛 성공확률")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FG.),
                 color='white',
                 position='fill',
                 binwidth = 5,
                 alpha=.7)+
  labs(title='포지션 별 2점 슛 성공확률 분포',
       y = "Count", 
       x = "2점 슛 성공확률")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FG.),
                 color='white',
                 position='stack',
                 binwidth = 5,
                 alpha=.7)+
  labs(title='포지션 별 2점 슛 성공확률 분포',
       y = "Count", 
       x = "2점 슛 성공확률")

#센터의 경우 높은 성공확률을 많이 보이고 가드의 경우는 낮은 성공확률을 보인다.


##### 포지션 별 boxplot(총 2점슛 성공 횟수, 시도횟수, 성공확률)
##### 총 2점 슛 성공 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(FGM)), 
             y=FGM, 
             fill=fct_reorder(Pos, desc(FGM))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 2점슛 성공 횟수 분포',
       y='총 2점슛 성공 횟수',
       x='포지션',
       fill='포지션')

#가드가 포워드보다 2점슛 성공 횟수가 높음을 보인다


##### 총 2점슛 시도 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(FGA)), 
             y = FGA, 
             fill = fct_reorder(Pos, desc(FGA))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 2점슛 시도 횟수 분포',
       y='총 2점슛 시도 횟수',
       x='포지션',
       fill='포지션')

#가드가 다른 포지션에 비해 2점 슛 시도 횟수가 높음을 알 수 있다

##### 2점슛 성공 확률
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(FG.)), 
             y = FG., 
             fill = fct_reorder(Pos, desc(FG.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 2점슛 성공 확률 분포',
       y='2점슛 성공 확률',
       x='포지션',
       fill='포지션')

#센터와 파워 포워드가 굉장히 높은 수치를 보이는데 골 밑에서 많이 시도하기 때문에 타당한 수치라고 판단할 수 있다.


#### 1-4) 총 3점 슛 성공 횟수,시도 횟수 및 성공 확률

##### 총 3점 슛 성공 횟수
stat %>% 
  ggplot(aes(X3PM))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 10)+
  labs(title='총 3점 슛 성공 횟수 분포',
       y = "Count", 
       x = "총 3점 슛 성공 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(X3PM),
                 color='white',
                 position='fill',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 3점 슛 성공 횟수 분포',
       y = "Count", 
       x = "총 3점 슛 성공 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(X3PM),
                 color='white',
                 position='stack',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 3점 슛 성공 횟수 분포',
       y = "Count", 
       x = "총 3점 슛 성공 횟수")

#0에 수치가 꽤 많음을 볼 수 있다. 자세히 살펴보자
#센터와 파워포워드의 경우 매우 낮은 수치를 갖는 경우가 많으며 슈팅 가드의 경우 높은 수치 쪽에 분포한다.

##### 총 3점 슛 시도 횟수
stat %>% 
  ggplot(aes(X3PA))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 20)+
  labs(title='총 3점 슛 시도 횟수 분포',
       y = "Count", 
       x = "총 3점 슛 시도 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(X3PA),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 3점 슛 시도 횟수 분포',
       y = "Count", 
       x = "총 3점 슛 시도 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(X3PA),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 3점 슛 시도 횟수 분포',
       y = "Count", 
       x = "총 3점 슛 시도 횟수")

#총 3점슛 성공 횟수와 비슷한 분포를 보인다


##### 3점슛 성공 확률
stat %>% 
  ggplot(aes(X3P.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 1)+
  labs(title='3점 슛 성공확률 분포',
       y = "Count", 
       x = "3점 슛 성공확률")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(X3P.),
                 color='white',
                 position='fill',
                 binwidth = 5,
                 alpha=.7)+
  labs(title='포지션 별 3점 슛 성공확률 분포',
       y = "Count", 
       x = "3점 슛 성공확률")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(X3P.),
                 color='white',
                 position='stack',
                 binwidth = 5,
                 alpha=.7)+
  labs(title='포지션 별 3점 슛 성공확률 분포',
       y = "Count", 
       x = "3점 슛 성공확률")


#0에 굉장히 많은 분포를 보인다. 이는 성공확률이 0이라기  보다 시도조차 안한 경우가 많이 있기 때문이다. 또한 이는 센터와 파워 포워드에 굉장히 많이 분포한다
stat %>% 
  filter(X3PA==0) %>% 
  select(Pos) %>% 
  table()

stat %>% 
  filter(X3PM==0) %>% 
  select(Pos) %>% 
  table()


##### 포지션 별 boxplot(총 3점슛 성공 횟수, 시도횟수, 성공확률)
##### 총 3점 슛 성공 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(X3PM)), 
             y=X3PM, 
             fill=fct_reorder(Pos, desc(X3PM))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 3점슛 성공 횟수 분포',
       y='총 3점슛 성공 횟수',
       x='포지션',
       fill='포지션')

#센터의 경우 거의 분포가 없으며, 파워포워드의 경우도 평균은 거의 0에 가깝다


#####총 3점슛 시도 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(X3PA)), 
             y = X3PA, 
             fill = fct_reorder(Pos, desc(X3PA))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 3점슛 시도 횟수 분포',
       y='총 3점슛 시도 횟수',
       x='포지션',
       fill='포지션')
#위와 같은 맥락


##### 3점슛 성공 확률
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(X3P.)), 
             y = X3P., 
             fill = fct_reorder(Pos, desc(X3P.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 3점슛 성공 확률 분포',
       y='3점슛 성공 확률',
       x='포지션',
       fill='포지션')

#센터는 평균이 0이고 파워포워드는 다른 포지션에 비해 매우 낮은 수치를 보인다



#### 1-5) 총 자유투 성공 횟수,시도 횟수 및 성공 확률

##### 총 자유투 성공 횟수
stat %>% 
  ggplot(aes(FTM))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 20)+
  labs(title='총 자유투 성공 횟수 분포',
       y = "Count", 
       x = "총 자유투 성공 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FTM),
                 color='white',
                 position='fill',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 자유투 성공 횟수 분포',
       y = "Count", 
       x = "총 자유투 성공 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FTM),
                 color='white',
                 position='stack',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 자유투 성공 횟수 분포',
       y = "Count", 
       x = "총 자유투 성공 횟수")

#큰 특징은 보이지 않는다

##### 총 자유투 시도 횟수
stat %>% 
  ggplot(aes(FTA))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 20)+
  labs(title='총 자유투 시도 횟수 분포',
       y = "Count", 
       x = "총 자유투 시도 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FTA),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 자유투 시도 횟수 분포',
       y = "Count", 
       x = "총 자유투 시도 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FTA),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 자유투 시도 횟수 분포',
       y = "Count", 
       x = "총 자유투 시도 횟수")

#큰 특징은 보이지 않는다


##### 자유투 성공 확률
stat %>% 
  ggplot(aes(FT.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 1)+
  labs(title='자유투 성공확률 분포',
       y = "Count", 
       x = "자유투 성공확률")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FT.),
                 color='white',
                 position='fill',
                 binwidth = 5,
                 alpha=.7)+
  labs(title='포지션 별 자유투 성공확률 분포',
       y = "Count", 
       x = "자유투 성공확률")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(FT.),
                 color='white',
                 position='stack',
                 binwidth = 5,
                 alpha=.7)+
  labs(title='포지션 별 자유투 성공확률 분포',
       y = "Count", 
       x = "자유투 성공확률")

#성공 확률이 0 또는 100의 경우가 꽤 보이는데 이는 시도 횟수가 아예 없거나 적은 경우임을 알 수 있음
stat %>% 
  filter(FT.==0 | FT.==100) %>% 
  select(FTA, FTM) %>% 
  table()
  


##### 포지션 별 boxplot(총 자유투 성공 횟수, 시도횟수, 성공확률)
##### 총 자유투 성공 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(FTM)), 
             y=FTM, 
             fill=fct_reorder(Pos, desc(FTM))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 자유투 성공 횟수 분포',
       y='총 자유투 성공 횟수',
       x='포지션',
       fill='포지션')

#큰 차이는 없지만 센터와 가드의 경우가 살짝 높은 수치를 보인다


##### 총 자유투 시도 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(FTA)), 
             y = FTA, 
             fill = fct_reorder(Pos, desc(FTA))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 자유투 시도 횟수 분포',
       y='총 자유투 시도 횟수',
       x='포지션',
       fill='포지션')

#위와 같다

##### 자유투 성공 확률
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(FT.)), 
             y = FT., 
             fill = fct_reorder(Pos, desc(FT.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 자유투 성공 확률 분포',
       y='자유투 성공 확률',
       x='포지션',
       fill='포지션')

#파워 포워드와 센터가 성공확률이 꽤나 낮은 것을 볼 수 있다.


#### 1-6) 총 공격 리바운드 횟수, 평균 공격 리바운드 횟수

##### 총 공격 리바운드 횟수
stat %>% 
  ggplot(aes(OREB))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 10)+
  labs(title='총 공격 리바운드 횟수 분포',
       y = "Count", 
       x = "총 공격 리바운드 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(OREB),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 공격 리바운드 횟수 분포',
       y = "Count", 
       x = "총 공격 리바운드 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(OREB),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 공격 리바운드 횟수 분포',
       y = "Count", 
       x = "총 공격 리바운드 횟수")

#센터와 파워포워드가 높은 수치를 갖는다는 것을 확인


##### 평균 공격 리바운드 횟수
stat %>% 
  ggplot(aes(OREB.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .5)+
  labs(title='평균 공격 리바운드 횟수 분포',
       y = "Count", 
       x = "평균 공격 리바운드 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(OREB.),
                 color='white',
                 position='fill',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 공격 리바운드 횟수 분포',
       y = "Count", 
       x = "평균 공격 리바운드 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(OREB.),
                 color='white',
                 position='stack',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 공격 리바운드 횟수 분포',
       y = "Count", 
       x = "평균 공격 리바운드 횟수")

#센터와 파워포워드가 높은 수치를 갖는다


##### 포지션 별 boxplot(총 공격 리바운드 횟수, 평균 공격 리바운드 횟수)
##### 총 공격 리바운드 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(OREB)), 
             y=OREB, 
             fill=fct_reorder(Pos, desc(OREB))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 공격 리바운드 횟수 분포',
       y='총 공격 리바운드 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드가 높은 수치를 보인다


##### 평균 공격 리바운드 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(OREB.)), 
             y = OREB., 
             fill = fct_reorder(Pos, desc(OREB.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 공격 리바운드 횟수 분포',
       y='평균 공격 리바운드 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드가 높은 수치를 보인다



#### 1-7) 총 수비 리바운드 횟수, 평균 수비 리바운드 횟수

##### 총 수비 리바운드 횟수
stat %>% 
  ggplot(aes(DREB))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 10)+
  labs(title='총 수비 리바운드 횟수 분포',
       y = "Count", 
       x = "총 수비 리바운드 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(DREB),
                 color='white',
                 position='fill',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 수비 리바운드 횟수 분포',
       y = "Count", 
       x = "총 수비 리바운드 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(DREB),
                 color='white',
                 position='stack',
                 binwidth = 100,
                 alpha=.7)+
  labs(title='포지션 별 총 수비 리바운드 횟수 분포',
       y = "Count", 
       x = "총 수비 리바운드 횟수")

#센터와 파워포워드가 높은 수치를 갖는다는 것을 확인


##### 평균 수비 리바운드 횟수
stat %>% 
  ggplot(aes(DREB.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .5)+
  labs(title='평균 수비 리바운드 횟수 분포',
       y = "Count", 
       x = "평균 수비 리바운드 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(DREB.),
                 color='white',
                 position='fill',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 수비 리바운드 횟수 분포',
       y = "Count", 
       x = "평균 수비 리바운드 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(DREB.),
                 color='white',
                 position='stack',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 수비 리바운드 횟수 분포',
       y = "Count", 
       x = "평균 수비 리바운드 횟수")

#센터와 파워포워드가 높은 수치를 갖는다


##### 포지션 별 boxplot(총 수비 리바운드 횟수, 평균 수비 리바운드 횟수)
##### 총 수비 리바운드 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(DREB)), 
             y=DREB, 
             fill=fct_reorder(Pos, desc(DREB))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 수비 리바운드 횟수 분포',
       y='총 수비 리바운드 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드가 높은 수치를 보인다


##### 평균 수비 리바운드 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(DREB.)), 
             y = DREB., 
             fill = fct_reorder(Pos, desc(DREB.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 수비 리바운드 횟수 분포',
       y='평균 수비 리바운드 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드가 높은 수치를 보인다



#### 1-8) 총 어시스트 횟수, 평균 어시스트 횟수

##### 총 어시스트 횟수
stat %>% 
  ggplot(aes(AST))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 20)+
  labs(title='총 어시스트 횟수 분포',
       y = "Count", 
       x = "총 어시스트 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(AST),
                 color='white',
                 position='fill',
                 binwidth = 40,
                 alpha=.7)+
  labs(title='포지션 별 총 어시스트 횟수 분포',
       y = "Count", 
       x = "총 어시스트 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(AST),
                 color='white',
                 position='stack',
                 binwidth = 40,
                 alpha=.7)+
  labs(title='포지션 별 총 어시스트 횟수 분포',
       y = "Count", 
       x = "총 어시스트 횟수")

#포인트 가드가 높은 수치를 포인다

##### 평균 어시스트 횟수
stat %>% 
  ggplot(aes(AST.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .5)+
  labs(title='평균 어시스트 횟수 분포',
       y = "Count", 
       x = "평균 어시스트 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(AST.),
                 color='white',
                 position='fill',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 어시스트 횟수 분포',
       y = "Count", 
       x = "평균 어시스트 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(AST.),
                 color='white',
                 position='stack',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 어시스트 분포',
       y = "Count", 
       x = "평균 어시스트 횟수")

#포인트 가드가 높은 수치를 보인다


##### 포지션 별 boxplot(총 어시스트 횟수, 평균 어시스트 횟수)
##### 총 어시스트 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(AST)), 
             y=AST, 
             fill=fct_reorder(Pos, desc(AST))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 어시스트 횟수 분포',
       y='총 어시스트 횟수',
       x='포지션',
       fill='포지션')

#포인트 가드가 매우 높은 수치를 보인다


##### 평균 어시스트 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(AST.)), 
             y = AST., 
             fill = fct_reorder(Pos, desc(AST.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 어시스트 횟수 분포',
       y='평균 어시스트 횟수',
       x='포지션',
       fill='포지션')

#포인트 가드가 매우 높은 수치를  보인다



#### 1-9) 총 스틸 횟수, 평균 스틸 횟수

##### 총 스틸 횟수
stat %>% 
  ggplot(aes(STL))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 10)+
  labs(title='총 스틸 횟수 분포',
       y = "Count", 
       x = "총 스틸 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(STL),
                 color='white',
                 position='fill',
                 binwidth = 10,
                 alpha=.7)+
  labs(title='포지션 별 총 스틸 횟수 분포',
       y = "Count", 
       x = "총 스틸 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(STL),
                 color='white',
                 position='stack',
                 binwidth = 10,
                 alpha=.7)+
  labs(title='포지션 별 총 스틸 횟수 분포',
       y = "Count", 
       x = "총 스틸 횟수")

#센터나 파워포워드는 높은 수치에는 분포하지 않음을 확인

##### 평균 스틸 횟수
stat %>% 
  ggplot(aes(STL.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .1)+
  labs(title='평균 스틸 횟수 분포',
       y = "Count", 
       x = "평균 스틸 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(STL.),
                 color='white',
                 position='fill',
                 binwidth = .1,
                 alpha=.7)+
  labs(title='포지션 별 평균 스틸 횟수 분포',
       y = "Count", 
       x = "평균 스틸 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(STL.),
                 color='white',
                 position='stack',
                 binwidth = .1,
                 alpha=.7)+
  labs(title='포지션 별 평균 스틸 분포',
       y = "Count", 
       x = "평균 스틸 횟수")

#센터나 파워포워드는 높은 수치에는 분포하지 않음을 확인


##### 포지션 별 boxplot(총 스틸 횟수, 평균 스틸 횟수)
##### 총 스틸 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(STL)), 
             y=STL, 
             fill=fct_reorder(Pos, desc(STL))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 스틸 횟수 분포',
       y='총 스틸 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드는 비교적 낮은 수치를 보인다


##### 평균 스틸 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(STL.)), 
             y = STL., 
             fill = fct_reorder(Pos, desc(STL.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 스틸 횟수 분포',
       y='평균 스틸 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드는 비교적 낮은 수치를 보인다



#### 1-10) 총 블락 횟수, 평균 블락 횟수

##### 총 블락 횟수
stat %>% 
  ggplot(aes(BLK))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 5)+
  labs(title='총 블락 횟수 분포',
       y = "Count", 
       x = "총 블락 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(BLK),
                 color='white',
                 position='fill',
                 binwidth = 10,
                 alpha=.7)+
  labs(title='포지션 별 총 블락 횟수 분포',
       y = "Count", 
       x = "총 블락 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(BLK),
                 color='white',
                 position='stack',
                 binwidth = 10,
                 alpha=.7)+
  labs(title='포지션 별 총 블락 횟수 분포',
       y = "Count", 
       x = "총 블락 횟수")

#가드는 낮은 수치, 센터와 파워 포워드는 높은 수치에 분포함을 확인

##### 평균 블락 횟수
stat %>% 
  ggplot(aes(BLK.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .1)+
  labs(title='평균 스틸 블락 분포',
       y = "Count", 
       x = "평균 블락 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(BLK.),
                 color='white',
                 position='fill',
                 binwidth = .1,
                 alpha=.7)+
  labs(title='포지션 별 평균 블락 횟수 분포',
       y = "Count", 
       x = "평균 블락 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(BLK.),
                 color='white',
                 position='stack',
                 binwidth = .1,
                 alpha=.7)+
  labs(title='포지션 별 평균 블락 분포',
       y = "Count", 
       x = "평균 블락 횟수")

#가드는 낮은 수치, 센터와 파워 포워드는 높은 수치에 분포함을 확인


##### 포지션 별 boxplot(총 블락 횟수, 평균 블락 횟수)
##### 총 블락 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(BLK)), 
             y=BLK, 
             fill=fct_reorder(Pos, desc(BLK))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 블락 횟수 분포',
       y='총 블락 횟수',
       x='포지션',
       fill='포지션')

#센터가 매우 높은 수치를 가짐을 확인


##### 평균 블락 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(BLK.)), 
             y = BLK., 
             fill = fct_reorder(Pos, desc(BLK.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 블락 횟수 분포',
       y='평균 블락 횟수',
       x='포지션',
       fill='포지션')

#센터가 매우 높은 수치를 가짐을 확인



#### 1-11) 총 턴오버 횟수, 평균 턴오버 횟수

##### 총 턴오버 횟수
stat %>% 
  ggplot(aes(TOV))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 5)+
  labs(title='총 턴오버 횟수 분포',
       y = "Count", 
       x = "총 턴오버 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(TOV),
                 color='white',
                 position='fill',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 턴오버 횟수 분포',
       y = "Count", 
       x = "총 턴오버 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(TOV),
                 color='white',
                 position='stack',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 턴오버 횟수 분포',
       y = "Count", 
       x = "총 턴오버 횟수")

#가드가 높은 수치, 센터와 파워포워드는 낮은 수치를 가짐을 확인. 하지만 센터는 100~150에서 비교적 높은 비율을 가진다

##### 평균 턴오버 횟수
stat %>% 
  ggplot(aes(TOV.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .1)+
  labs(title='평균 스틸 턴오버 분포',
       y = "Count", 
       x = "평균 턴오버 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(TOV.),
                 color='white',
                 position='fill',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 턴오버 횟수 분포',
       y = "Count", 
       x = "평균 턴오버 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(TOV.),
                 color='white',
                 position='stack',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 턴오버 분포',
       y = "Count", 
       x = "평균 턴오버 횟수")

#가드가 높은 수치, 센터와 파워포워드는 낮은 수치를 가짐을 확인


##### 포지션 별 boxplot(총 턴오버 횟수, 평균 턴오버 횟수)
##### 총 턴오버 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(TOV)), 
             y=TOV, 
             fill=fct_reorder(Pos, desc(TOV))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 턴오버 횟수 분포',
       y='총 턴오버 횟수',
       x='포지션',
       fill='포지션')

#포인트가드와 센터가 비교적 높은 수치를 가진다


##### 평균 턴오버 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(TOV.)), 
             y = TOV., 
             fill = fct_reorder(Pos, desc(TOV.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 턴오버 횟수 분포',
       y='평균 턴오버 횟수',
       x='포지션',
       fill='포지션')

#포인트가드와 센터 그리고 슈팅가드가 비교적 높은 수치를 가진다



#### 1-12) 총 파울 횟수, 평균 파울 횟수

##### 총 파울 횟수
stat %>% 
  ggplot(aes(PF))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = 5)+
  labs(title='총 파울 횟수 분포',
       y = "Count", 
       x = "총 파울 횟수")


stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PF),
                 color='white',
                 position='fill',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 파울 횟수 분포',
       y = "Count", 
       x = "총 파울 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PF),
                 color='white',
                 position='stack',
                 binwidth = 20,
                 alpha=.7)+
  labs(title='포지션 별 총 파울 횟수 분포',
       y = "Count", 
       x = "총 파울 횟수")

#센터가 비교적 높은 수치에서 분포를 가진다

##### 평균 파울 횟수
stat %>% 
  ggplot(aes(PF.))+
  geom_histogram(fill = "cornflowerblue", 
                 color='white',
                 binwidth = .1)+
  labs(title='평균 스틸 파울 분포',
       y = "Count", 
       x = "평균 파울 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PF.),
                 color='white',
                 position='fill',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 파울 횟수 분포',
       y = "Count", 
       x = "평균 파울 횟수")

stat %>% 
  ggplot(aes(fill=Pos))+
  geom_histogram(aes(PF.),
                 color='white',
                 position='stack',
                 binwidth = .5,
                 alpha=.7)+
  labs(title='포지션 별 평균 파울 분포',
       y = "Count", 
       x = "평균 파울 횟수")

#센터와 파워포워드가 비교적 높은 수치에서 분포를 가진다


##### 포지션 별 boxplot(총 파울 횟수, 평균 파울 횟수)
##### 총 파울 횟수
stat %>% 
  ggplot(aes(x=fct_reorder(Pos, desc(PF)), 
             y=PF, 
             fill=fct_reorder(Pos, desc(PF))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 총 파울 횟수 분포',
       y='총 파울 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드가 비교적 높은 수치를 가진다


##### 평균 파울 횟수
stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(PF.)), 
             y = PF., 
             fill = fct_reorder(Pos, desc(PF.))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 평균 파울 횟수 분포',
       y='평균 파울 횟수',
       x='포지션',
       fill='포지션')

#센터와 파워포워드가 비교적 높은 수치를 보인다














