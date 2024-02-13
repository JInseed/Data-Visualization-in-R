library(tidyverse)
install.packages('ggthemes')
library(ggthemes)
stat=read.csv('stat.csv',header=TRUE)

### 2.salary와 변수 간의 관계(포지션별)
####2-1)총 출전 시간, 평균 출전 시간

#####총 출전 시간
stat %>% 
  ggplot(aes(MIN,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 출전 시간과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 출전 시간")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(MIN,log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 출전 시간과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 출전 시간")+
  scale_y_continuous(labels = scales::comma)

#총 출전 시간과 salary에서 양의 관계를 살펴 볼 수 있다. 포지션 별로도 크게 다르지는 않다. salary에 로그변환을 하면 선형성을 안정적으로 확보할  수 있을 것으로 보인다

######평균 출전 시간
stat %>% 
  ggplot(aes(MIN.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              lty=7,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 출전 시간과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 출전 시간")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(MIN.,log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              lty=7,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 출전 시간과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 출전 시간")+
  scale_y_continuous(labels = scales::comma)

#평균 출전 시간과 salary에서 양의 관계를 볼 수 있다. 포지션 별로도 크게  다르지는 않다. salary에 로그변환을 하면 선형성을 안정적으로 확보할 수 있을 것으로 보인다

####2-2)총 골 넣은 점수, 평균 골 넣은 점수

#####총 골 넣은 점수
stat %>% 
  ggplot(aes(PTS,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 골 넣은 점수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 골 넣은 점수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(PTS+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 골 넣은 점수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 골 넣은 점수")+
  scale_y_continuous(labels = scales::comma)


#총 골 넣은 점수와 salary와 양의 상관관계를 보인다. 포지션 별로도  크게 다르지는 않다. 다만 점수가 높아질 수록 퍼지는 정도가 매우 커짐에 이분산성을 유의해야할듯 하다.
#로그 변환시 안정성이 어느정도 확보된다


######평균 골 넣은 점수
stat %>% 
  ggplot(aes(log(PTS.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 골 넣은 점수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 골 넣은 점수")+
  scale_y_continuous(labels = scales::comma)

#평균 골 넣은 점수와 salary와 양의 상관관계를 보인다. 포지션 별로도  크게 다르지는 않다.
#로그변환시 안정성이 어느정도 확보된다

####2-3)총 2점 슛 성공 횟수,시도 횟수 및 성공 확률

#####총 2점 슛 성공 횟수
stat %>% 
  ggplot(aes(FGM, salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 2점 슛 성공 횟수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 2점 슛 성공 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(FGM, log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 2점 슛 성공 횟수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 2점 슛 성공 횟수")+
  scale_y_continuous(labels = scales::comma)

#총 2점 슛  성공 횟수와 salary와의 관계는 양의 상관관계를 보인다. 포지션별로도 크게 다르지 않으나 센터와 스몰포워드의 경우 끝 선에서 굉장이 다이나믹하게 움직이는데 이는 성공 횟수가 많은 곳에는 이들이 매우 적게 분포하기 때문이다.

######총 2점 슛 시도 횟수
stat %>% 
  ggplot(aes(FGA,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 2점 슛 시도 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 2점 슛 시도 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(FGA,log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 2점 슛 시도 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 2점 슛 시도 횟수")+
  scale_y_continuous(labels = scales::comma)


#총 2점 슛 시도 횟수와 salary와의 관계는 양의 상관관계를 보인다. 포지션별로도 크게 다르지 않으나 센터의 경우 위와 같이 횟수가 많은 쪽에는 매우 적게 분포하여 선의 휜 것을 살펴볼  수 있다

######2점슛 성공 확률
stat %>% 
  ggplot(aes(FG.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              lty=7,
              se=FALSE, 
              span=5,
              size=1.2)+
  labs(title='2점슛 성공 확률과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "2점슛 성공 확률")+
  scale_y_continuous(labels = scales::comma)

#음의 상관관계는 아니지만 linear model에 넣기에는 부적합해 보인다
cor(stat$FG.,stat$salary)
cor(stat$FG.,log2(stat$salary))

stat %>% 
  ggplot(aes(FG.,log2(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              lty=7,
              se=FALSE, 
              span=5,
              size=1.2)+
  labs(title='2점슛 성공 확률과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "2점슛 성공 확률")+
  scale_y_continuous(labels = scales::comma)

#로그변환을 통해서 상관계수를 높일 수는 있으나 그래프에서 원형이 나타나 linear model에는 부적합해 보인다



####2-4)총 3점 슛 성공 횟수,시도 횟수 및 성공 확률

#####총 3점 슛 성공 횟수
stat %>% 
  ggplot(aes(X3PM, salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 3점 슛 성공 횟수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 3점 슛 성공 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(X3PM, log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 3점 슛 성공 횟수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 3점 슛 성공 횟수")+
  scale_y_continuous(labels = scales::comma)


#센터와 파워포워드의 경우 0에 가까운 곳에 굉장히 많이 분포한다. 하지만 나머지 포지션의 경우는 어느정도 양의 상관관계를 보인다. 전체적으로 봤을 때는 양의 상관관계라고 하기에 애매해서 main effect는 필요없으나 interaction term은 필요한 특이한 케이스 일지도 모른다는 걸 시사한다.

######총 3점 슛 시도 횟수
stat %>% 
  ggplot(aes(X3PA,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 3점 슛 시도 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 3점 슛 시도 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(X3PA,log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 3점 슛 시도 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 3점 슛 시도 횟수")+
  scale_y_continuous(labels = scales::comma)

#위와 비슷한 결과가 나왔다

######3점슛 성공 확률
stat %>% 
  ggplot(aes(X3P.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              lty=7,
              se=FALSE, 
              span=5,
              size=1.2)+
  labs(title='3점슛 성공 확률과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "3점슛 성공 확률")+
  scale_y_continuous(labels = scales::comma)

#0에 분포하는 경우가 굉장히 많으며 3점슛 성공확률과 salary간에 상관관계가 있다고 하기는 어렵다.


####2-5)총 자유투 성공 횟수,시도 횟수 및 성공 확률

#####총 자유투 성공 횟수
stat %>% 
  ggplot(aes(FTM, salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 자유투 성공 횟수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 자유투 성공 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(FTM), log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 자유투 성공 횟수과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 자유투 성공 횟수")+
  scale_y_continuous(labels = scales::comma)


#어느정도 자유투 성공횟수와 salary에는 양의 상관관계를 보인다.다만 이분산성이 크게 나타난다. 포지션 별로 크게 다르지 않다
#로그변환을 통해 안정화 할 수 있다. 


######총 자유투 시도 횟수
stat %>% 
  ggplot(aes(FTA,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 자유투 시도 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 자유투 시도 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(FTA),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 자유투 시도 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 자유투 시도 횟수")+
  scale_y_continuous(labels = scales::comma)


#어느정도 자유투 성공횟수와 salary에는 양의 상관관계를 보인다.다만 이분산성이 크게 나타난다. 포지션 별로 크게  다르지 않다
#로그변환을 통해 안정화 할 수 있다. 

######자유투 성공 확률
stat %>% 
  ggplot(aes(FT.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              lty=7,
              se=FALSE, 
              span=5,
              size=1.2)+
  labs(title='자유투 성공 확률과 salary와의 관계(포지션별)',
       y = "salary", 
       x = "자유투 성공 확률")+
  scale_y_continuous(labels = scales::comma)

#자유투 성공 확률과 salary와 상관관계가 있다고 하기는 어렵다



####2-6)총 공격 리바운드 횟수, 평균 공격 리바운드 횟수

#####총 공격 리바운드 횟수
stat %>% 
  ggplot(aes(OREB,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 공격 리바운드 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 공격 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(OREB+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 공격 리바운드 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 공격 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)



#센터와 파워포워드의 경우 어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 공격 리바운드 횟수
stat %>% 
  ggplot(aes(OREB.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 공격 리바운드 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 공격 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(OREB.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 공격 리바운드 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 공격 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)


#평균 공격 리바운드 횟수와 salary 와는 정말 약한 양의 상관관계를 보인다고 할 수는 있으나 이분산성이 매우 크다. 또한 포지션별로 상이한 모습을 보인다
#로그변환으로 어느정도 안정성을 확보할 수 있다


####2-7)총 수비 리바운드 횟수, 평균 수비 리바운드 횟수

#####총 수비 리바운드 횟수
stat %>% 
  ggplot(aes(DREB,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 수비 리바운드 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 수비 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(DREB+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 수비 리바운드 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 수비 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)



#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로 기울기가 다른 것을 확인
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 수비 리바운드 횟수
stat %>% 
  ggplot(aes(DREB.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 수비 리바운드 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 수비 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(DREB.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 수비 리바운드 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 수비 리바운드 횟수")+
  scale_y_continuous(labels = scales::comma)


#평균 수비 리바운드 횟수와 salary 와는 양의 상관관계를 보인다고 할 수는 있으나 이분산성이 매우 크다. 포지션별로 기울기가 다른 것을 확인.
#로그변환으로 어느정도 안정성을 확보할 수 있다


####2-8)총 어시스트 횟수, 평균 어시스트 횟수

#####총 어시스트 횟수
stat %>% 
  ggplot(aes(AST,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 어시스트 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 어시스트 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(AST+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 어시스트 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 어시스트 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로 분포가 상이하다.
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 어시스트 횟수
stat %>% 
  ggplot(aes(AST.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 어시스트 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 어시스트 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(AST.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 어시스트와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 어시스트 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로도 상이함을 볼 수 있다
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


####2-9)총 스틸 횟수, 평균 스틸 횟수

#####총 스틸 횟수
stat %>% 
  ggplot(aes(STL,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 스틸 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 스틸 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(STL+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 스틸 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 스틸 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로 조금 상이함을 보인다
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 스틸 횟수
stat %>% 
  ggplot(aes(STL.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 스틸 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 스틸 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(STL.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 스틸와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 스틸 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로도 조금 상이함을 볼 수 있다
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


####2-10)총 블락 횟수, 평균 블락 횟수

#####총 블락 횟수
stat %>% 
  ggplot(aes(BLK,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 블락 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 블락 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(BLK+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 블락 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 블락 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 센터와 파워 포워드의 경우가 높은 수치에 분포함을 확인할 수 있다
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 블락 횟수
stat %>% 
  ggplot(aes(BLK.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 블락 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 블락 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(BLK.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 블락와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 블락 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로도 상이함을 볼 수 있다
#로그변환으로 안정성을 어느 정도 확보 할 수 있다



####2-11)총 턴오버 횟수, 평균 턴오버 횟수

#####총 턴오버 횟수
stat %>% 
  ggplot(aes(TOV,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 턴오버 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 턴오버 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(TOV+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 턴오버 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 턴오버 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로 크게 다르지 않다 
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 턴오버 횟수
stat %>% 
  ggplot(aes(TOV.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 턴오버 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 턴오버 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(TOV.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 턴오버와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 턴오버 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 크다. 포지션 별로 기울기는 크게 다르지는 않지만 위치가 조금 다르다.
#로그변환으로 안정성을 어느 정도 확보 할 수 있다

####2-12)총 파울 횟수, 평균 파울 횟수

#####총 파울 횟수
stat %>% 
  ggplot(aes(PF,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 파울 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 파울 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(PF,log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              size=1.2,
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='총 파울 횟수 salary와의 관계(포지션별)',
       y = "salary", 
       x = "총 파울 횟수")+
  scale_y_continuous(labels = scales::comma)


#어느 정도 양의 상관관계를 보이나 이분상성이 매우 크다. 포지션 별로 크게 다르지 않다 
#로그변환으로 안정성을 어느 정도 확보 할 수 있다


######평균 파울 횟수
stat %>% 
  ggplot(aes(PF.,salary, color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 파울 횟수와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 파울 횟수")+
  scale_y_continuous(labels = scales::comma)

stat %>% 
  ggplot(aes(log(PF.+1),log(salary), color=Pos))+
  geom_point(alpha=.4,
             size=3)+
  geom_smooth(method='loess',
              se=FALSE, 
              span=.7,
              size=1.2)+
  labs(title='평균 파울와 salary와의 관계(포지션별)',
       y = "salary", 
       x = "평균 파울 횟수")+
  scale_y_continuous(labels = scales::comma)

#어느 정도 양의 상관관계를 보이나 이분상성이 크고 원형에 가깝다. 포지션 별로 크게 다르지 않다
#로그변환으로 안정성을 어느 정도 확보 할 수 있으나 linear model에는 적합하지 않다고 판단한다.








