library(modelr)
#앞선 과정에서 각 변수들과 salary의 관계를 살펴보고 linear model을 생성하기 위해 변수변환과 포지션과의 interaction term이 필요함을 알 수 있었다. 또한 salary와 관계가 없다고 판단한 변수도 존재했다. 따라서 필요없는 변수는 제외하고서 main effect model과  interaction term 을 추가한 model을 비교해본다. modeling에서 제외한 변수들은 비교하는 분석에서도  제외한다

#### modeling에서 제외한 변수 : 2점슛 성공확롤, 3점슛 성공확률, 자유투 성공확률, 평균 파울 횟수

mod1=lm(log(salary) ~ MIN+MIN.+log(PTS+1)+PTS.+FGM+FGA+X3PM+X3PA+
          log(FTM+1)+log(FTA+1)+log(OREB+1)+log(OREB.+1)+
          log(DREB+1)+log(DREB.+1)+log(AST+1)+log(AST.+1)+
          log(STL+1)+log(STL.+1)+log(BLK+1)+log(BLK.+1)+
          log(TOV+1)+log(TOV.+1)+PF+Pos,
        data=stat)

mod2=lm(log(salary) ~ (MIN+MIN.+log(PTS+1)+PTS.+FGM+FGA+X3PM+X3PA+
                       log(FTM+1)+log(FTA+1)+log(OREB+1)+
                       log(OREB.+1)+log(DREB+1)+log(DREB.+1)+
                       log(AST+1)+log(AST.+1)+log(STL+1)+
                       log(STL.+1)+log(BLK+1)+log(BLK.+1)+
                       log(TOV+1)+log(TOV.+1)+PF)*Pos+Pos,
        data=stat)


stat_mod=stat %>% 
  gather_residuals('without interaction'=mod1, 
                   'within interaction'=mod2)

summary(mod1) #R squared = 0.5198
summary(mod2) #R squared = 0.6962
#interaction term이 있는 model이 그러지 않는 것보다 전체 변동을 17.64% 더 잘 설명한다.

###3.잔차와 각 변수 관계
####3-1)총 출전 시간, 평균 출전 시간
#####총 출전 시간
stat_mod %>% 
  ggplot(aes(MIN, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 출전 시간과 잔차, model 간 비교',
       y = "잔차", 
       x = "총 출전 시간")

#### 평균 출전 시간
stat_mod %>% 
  ggplot(aes(MIN., resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 출전 시간과 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 출전 시간")

#interaction term이 있는 쪽이 확실히 잔차의 안정성이 높고 둘다 무작위성이 잘 보여지므로 model이 잘 생성되었음을 알 수  있다. 또한 interaction term이 없는 쪽에서는 포지션 별로 보면 패턴이 조금 나타남을 알 수 있다.


####3-2)총 골 넣은 점수, 평균 골 넣은 점수
#####총 골 넣은 점수
stat_mod %>% 
  ggplot(aes(PTS, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 골 넣은 점수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 골 넣은 점수")

#### 평균 골 넣은 점수
stat_mod %>% 
  ggplot(aes(PTS., resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 골 넣은 점수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 골 넣은 점수")

#잔차의 이분산성이 조금 보이나 제외하고, interaction term이 있는 쪽이 잔차의 안정성이 높고 interaction term이 없는 쪽에서는 포지션 간 패턴이 조금 보인다

####3-3)총 2점 슛 성공 횟수,시도 횟수
#####총 2점 슛 성공 횟수
stat_mod %>% 
  ggplot(aes(FGM, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 2점 슛 성공 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 2점 슛 성공 횟수")

#### 총 2점 슛 시도 횟수
stat_mod %>% 
  ggplot(aes(FGA, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 2점 슛 시도 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 2점 슛 시도 횟수")

#잔차의 이분산성이 조금 보이나 제외하고, interaction term이 있는 쪽이 잔차의 안정성이 높고 interaction term이 없는 쪽에서는 포지션 간 패턴이 아주 조금 보이지만 무시해도 될 정도로 보인다.

####3-4)총 3점 슛 성공 횟수,시도 횟수
#####총 3점 슛 성공 횟수
stat_mod %>% 
  ggplot(aes(X3PM, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 3점 슛 성공 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 3점 슛 성공 횟수")

#### 총 3점 슛 시도 횟수
stat_mod %>% 
  ggplot(aes(X3PA, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 3점 슛 시도 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 3점 슛 시도 횟수")

#잔차의 이분산성이 보이나 제외하고, interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 잔차의 무작위성도 어느만큼 나타난다.

####3-5)총 자유투 성공 횟수,시도 횟수
#####총 자유투 성공 횟수
stat_mod %>% 
  ggplot(aes(log(FTM+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 자유투 성공 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 자유투 성공 횟수(log)")

#### 총 자유투 시도 횟수
stat_mod %>% 
  ggplot(aes(log(FTA+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 자유투 시도 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 자유투 시도 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단.


####3-6)총 공격 리바운드 횟수, 평균 공격 리바운드 횟수
#####총 공격 리바운드 횟수
stat_mod %>% 
  ggplot(aes(log(OREB+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 공격 리바운드 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 공격 리바운드 횟수(log)")

#### 평균 공격 리바운드 횟수
stat_mod %>% 
  ggplot(aes(log(OREB.+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 공격 리바운드 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 공격 리바운드 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단.

####3-7)총 수비 리바운드 횟수, 평균 수비 리바운드 횟수
#####총 수비 리바운드 횟수
stat_mod %>% 
  ggplot(aes(log(DREB+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 수비 리바운드 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 수비 리바운드 횟수(log)")

#### 평균 수비 리바운드 횟수
stat_mod %>% 
  ggplot(aes(log(DREB.+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 수비 리바운드 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 수비 리바운드 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 포지션 마다 살짝 패턴이 보이지만 무시해도 될 정도라고 판단.

####3-8)총 어시스트 횟수, 평균 어시스트 횟수
#####총 어시스트 횟수
stat_mod %>% 
  ggplot(aes(log(AST+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 어시스트 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 어시스트 횟수(log)")

#### 평균 어시스트 횟수
stat_mod %>% 
  ggplot(aes(log(AST.+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 어시스트 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 어시스트 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단.

####3-9)총 스틸 횟수, 평균 스틸 횟수
#####총 스틸 횟수
stat_mod %>% 
  ggplot(aes(log(STL+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 스틸 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 스틸 횟수(log)")

#### 평균 스틸 횟수
stat_mod %>% 
  ggplot(aes(log(STL.+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 스틸 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 스틸 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단.

####3-10)총 블락 횟수, 평균 블락 횟수
#####총 블락 횟수
stat_mod %>% 
  ggplot(aes(log(BLK+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 블락 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 블락 횟수(log)")

#### 평균 블락 횟수
stat_mod %>% 
  ggplot(aes(log(BLK.+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 블락 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 블락 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단. interaction term 이 없을 때는 포지션간 기울기가 조금 다른 것을 확인.

####3-11)총 턴오버 횟수, 평균 턴오버 횟수
#####총 턴오버 횟수
stat_mod %>% 
  ggplot(aes(log(TOV+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 턴오버 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 턴오버 횟수(log)")

#### 평균 턴오버 횟수
stat_mod %>% 
  ggplot(aes(log(TOV.+1), resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='평균 턴오버 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "평균 턴오버 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단.

####3-12)총 파울 횟수
#####총 파울 횟수
stat_mod %>% 
  ggplot(aes(PF, resid, color=Pos))+
  geom_point(alpha=.4,
             size=2.3)+
  geom_smooth(se=FALSE,
              size=1,
              method='loess',
              span=1)+
  facet_wrap(~model)+
  labs(title='총 파울 횟수와 잔차, model 간 비교',
       y = "잔차", 
       x = "총 파울 횟수(log)")

#interaction term이 있는 쪽이 잔차의 안정성이 높은 것을 확인할 수 있다. 무작위성도 잘 보여주어 model이 잘 생성되었다고 판단.



#interaction term을 추가 했을 때 잔차의 안정성이 더 좋아지며 interaction term이 없는 model에서 보이는 패턴 또한 잡아낼 수 있다는 것을 알 수 있었다. 추가적으로 스탯마다 interaction term이 필요한 것도 있고 없는 것도 있는데 이 또한 추가적으로 분석해보았으면 좋았을 거 같다. 또한 포지션에 따라서 각 스탯에 차이가 있는 것과 없는 것 또한 EDA에서 발견 되었는데 이 또한 추가적으로 분석해보았으면 좋았을 거 같다. 마지막으로 salary를 예측하는 주요한 변수들을 살펴보면 거의 다 출전 시간이 많으면 자연스럽게 높아지는 관계임을 알 수 있다.

#대표적으로 포지션에 따라 상이한 분포를 나타내는 총 블락횟수를 확인해보자

mod_BLK1=lm(BLK~Pos, data=stat)
mod_BLK2=lm(BLK~Pos+MIN, data=stat)
mod_BLK3=lm(BLK~Pos*MIN, data=stat)

p1=stat %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(BLK)), 
             y = BLK, 
             fill = fct_reorder(Pos, desc(BLK))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
    labs(title='포지션 별 블락 횟수 분포',
         y='총 블락 횟수',
         x='포지션',
         fill='포지션')
  

p2=stat %>%
  gather_residuals(mod_BLK1) %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(resid)), 
             y = resid, 
             fill = fct_reorder(Pos, desc(resid))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 블락 횟수 분포(포지션만 고려)',
       y='총 블락 횟수(포지션만 고려)',
       x='포지션',
       fill='포지션')

p3=stat %>%
  gather_residuals(mod_BLK2) %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(resid)), 
             y = resid, 
             fill = fct_reorder(Pos, desc(resid))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 블락 횟수 분포(포지션+MIN 고려)',
       y='총 블락 횟수(포지션+MIN 고려)',
       x='포지션',
       fill='포지션')

p4=stat %>%
  gather_residuals(mod_BLK3) %>% 
  ggplot(aes(x = fct_reorder(Pos, desc(resid)), 
             y = resid, 
             fill = fct_reorder(Pos, desc(resid))))+
  geom_violin(fill='lavender', alpha=.5)+
  geom_boxplot(width=.5,
               varwidth = TRUE,
               alpha=.5)+
  labs(title='포지션 별 블락 횟수 분포(포지션, 총 출전시간 interaction term 고려)',
       y='총 블락 횟수(포지션, 총 출전시간 interaction term 고려)',
       x='포지션',
       fill='포지션')

grid.arrange(p1,p2,p3,p4, nrow=2)

summary(mod_BLK1) #0.3059
summary(mod_BLK2) #0.533
summary(mod_BLK3) #0.6354
#R-squared를 확인하며 포지션이 전체 변동을 설명할 때 좋은 변수이며 총 출전 시간도 유의미한 변수임을 알 수 있다







