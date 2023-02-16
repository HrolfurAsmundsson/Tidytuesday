library(tidyverse)
library(tidytuesdayR)

#load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 7)
#save it locally
write_csv(tuesdata$age_gap,"age_gap.csv")


#code for the scatter plot

scatter_plot <- data %>%
  mutate(samesex=data$character_1_gender==data$character_2_gender) %>%
  mutate(relative_gap = if_else(character_1_gender=="man",age_difference,-age_difference)) %>% 
  ggplot(aes(x=release_year,y=relative_gap,color=character_1_gender,shape=samesex))+
  geom_point(alpha=0.5)+
  theme_minimal()+
  scale_x_discrete(name ="Release Year", 
                   limits=seq(1940,2023,10))+
  scale_y_discrete(name ="Age difference in years", 
                   limits=seq(-50,50,10),labels=abs(seq(-50,50,10)))+
  scale_color_manual(values=c("man"="blue","woman"="red"),labels=c("The man","The woman"))+
  labs(title="Hollywood Age Gap",color="Who is Older?",shape="Same sex couple",caption="Tidytuesday 2023-02-14: Data from hollywoodagegap.com")

scatter_plot

# code for the ribbon plot

ribbon_plot<- tibble(movie = c(data$movie_name,data$movie_name),
            year = c(data$release_year,data$release_year),
            age = c(data$actor_1_age,data$actor_2_age),
            gender = c(data$character_1_gender,data$character_2_gender)) %>% 
  group_by(year,gender) %>% 
  summarise(mean_age = mean(age)) %>% 
  pivot_wider(names_from = gender, values_from = mean_age) %>% 
  mutate(higer = ifelse(man>woman,"man","woman")) %>% 
  ggplot(alpha=0.5) +
  geom_ribbon(aes(x=year,ymin=woman,ymax=pmin(man,woman),fill="Woman"))+
  geom_line(aes(x=year,y=man),color="blue",width=2)+
  geom_line(aes(x=year,y=woman),color="red",width=2)+
  geom_ribbon(aes(x=year,ymin=man,ymax=pmin(man,woman),fill="Man"))+
  theme_minimal()+
  scale_x_discrete(name ="Release Year", 
                   limits=seq(1940,2023,10))+
  scale_y_discrete(name ="Mean Age", 
                   limits=seq(-50,50,10),labels=abs(seq(-50,50,10)))+
  scale_fill_manual(values=c("Man"="lightblue","Woman"="pink"),labels=c("Men","Women"))+
  labs(title="Hollywood Age Gap",fill="Which are Older on Average?",shape="Same sex couple",caption="Tidytuesday 2023-02-14: Data from hollywoodagegap.com")
ribbon_plot


# code for the distributions

#first we fit the distributions, I am sure this can be done in a nicer (tidier) way...
Emperical_DF <- tibble(movie = c(data$movie_name,data$movie_name),
                       year = c(data$release_year,data$release_year),
                       age = c(data$actor_1_age,data$actor_2_age),
                       gender = c(data$character_1_gender,data$character_2_gender)) %>% 
  group_by(gender) %>% summarise(edf = list(ecdf(age)))

# then we create a tibble with age and the corresponding ecdf value and plot.
distribution_plot <- tibble(age = rep(17:90,2),
             p = c(Emperical_DF$edf[1][[1]](17:90),Emperical_DF$edf[2][[1]](17:90)),
             gender = rep(c("man","woman"),each=74)) %>% 
  ggplot(aes(x=age,y=p,color=gender))+
  geom_line(size=1)+
  scale_color_manual(values=c("man"="blue","woman"="red"))+
  theme_minimal()+
  scale_x_discrete(name ="Actors age", 
                   limits=seq(15,90,5))+
  labs(title="Hollywood Age Gap",fill="Which are Older on Average?",shape="Same sex couple",caption="Tidytuesday 2023-02-14: Data from hollywoodagegap.com")+
  ylab(expression(P(x < age)))
distribution_plot