rm(list=ls())
library(tidyverse)

#데이터 불러오기
df=read.csv('C:/Users/82102/Desktop/project_데분시/players_stats2014.csv',header=T)

colSums(is.na(df))
head(df)
df %>% 
  count(Name) %>% 
  filter(n>1) #중복된 선수 없음 확인

#사용하지 않을 열 삭제
df2=df %>% 
  select(-c('Birthdate', 'Birth_Place' ,'Collage', 'Experience','AST.TOV','STL.TOV','EFF'))


#salary data, 2014-2015년도의 기량으로 2015-2016 salary가 정해졌다고 볼 수 있지만 2015-2016의 salary 데이터를 이용하여 2014-2015의 선수들의 stat을 비교하면 누락치가 많이 나와 2014-2015의 salary를 이용하여 분석 진행.
salary=read.csv('C:/Users/82102/Desktop/project_데분시/Salary.csv',header=T)

salary_2=salary %>% 
  filter(season=='2014-2015')

#stat, salary 의 선수명 이름 Name으로 바꾸고 데이터 형식이 A.J 인게 salary에선 AJ 이런게 있어서 .을 아예 빼서 join 
df2=df2 %>% 
  mutate(Name=str_replace_all(Name, '\\.', ''))

salary_3=salary_2 %>% 
  mutate(Name=str_replace_all(player, '\\.', ''))

join=df2 %>%
  left_join(salary_3, by = "Name")

view(salary_3)
#salary 데이터가 없는 선수는 빼고, 중복되거나 필요없는 열 삭제

colSums(is.na(join2))

join=join %>% 
  select(-c('team','player','season','position'))

join2=join[!is.na(join$salary),]

view(join2[apply(join2=='',1,any),])
#Age, Height, Weight, BMI, Pos, Team이 동시에 빈 값인 것을 확인하여 그 행들은 결측치로 간주하여 제거

stat2=join[!is.na(join$salary) & !apply(join=='',1,any),]


#PTS,MIN,OREB,BREB,REB,AST,BLK,TOV,PF의 경우 총 개수로 산정되어 있음. 게임 뛰는 시간이 많은 선수는 자연스럽게 각 개수가 많아 질 것. 따라서 실력을 측정할 수 있도록 각각을 게임 수로 나누어 새로운 변수 생성. 물론 게임을 많이 뛰는 선수가 더 좋은 선수일 가능성이 높으니 고려하여 분석 진행. 또한 공격 리바운드+수비 리바운드는 총 리바운드이기 때문에 총 리바운드는 분석에서 제외 한다.신체와 관련된 변수도 분석에서는 제외

stat2=stat2 %>% 
  mutate(PTS.=PTS/Games.Played,
         MIN.=MIN/Games.Played,
         OREB.=OREB/Games.Played,
         DREB.=DREB/Games.Played,
         REB.=REB/Games.Played,
         AST.=AST/Games.Played,
         STL.=STL/Games.Played,
         BLK.=BLK/Games.Played,
         TOV.=TOV/Games.Played,
         PF.=PF/Games.Played,)


stat=stat2 %>% 
  select(c('Name','Games.Played','MIN','MIN.','PTS','PTS.','FGM','FGA','FG.','X3PM','X3PA','X3P.','FTM','FTA','FT.','OREB','OREB.','DREB','DREB.','REB','REB.','AST','AST.','STL','STL.','BLK','BLK.','TOV','TOV.','PF','PF.','Pos','Team','Age','Height','Weight','BMI','salary'))


stat$Pos=as.factor(stat$Pos)

write.csv(stat,file='stat.csv',row.names = F)

class(stat$Pos)


