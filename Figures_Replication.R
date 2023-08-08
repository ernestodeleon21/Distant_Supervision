
library(tidyverse)

all.short = read.csv('all_results_summary.csv')

all.short = all.short %>% dplyr::select(!X) #Removing column


########## Manual results ##############


all.manual = all.short %>% 
  filter(Test_Type == 'Manual Labels Train/Test Split')

all.manual = all.manual %>% dplyr::select(!Test_Type)


all.manual2 = all.manual %>% pivot_longer(!c(Topic,model),
  names_to = "Measure",
  values_to = "count"
)

all.manual2$Topic = factor(all.manual2$Topic, 
                                     levels = c( 
                                                "Economy","Entertainment","Sports","Politics"))

all.manual2 = all.manual2 %>% mutate(count = ifelse(count == 0.00,0,count))

p1=ggplot(all.manual2, aes(x = count, y = Topic, color=Measure, shape=Measure)) + 
  geom_point(size=3) +
  facet_grid(. ~ model) + theme_bw() + xlab("")+
    theme(legend.position="bottom") + scale_colour_grey(start = 0, end = .7) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "0.25", "0.5", "0.75", "1.0"))

p1



path = 'manual_results.jpeg'

ggsave(path, plot = p1, height = 4, width = 7.5, units = "in", device='jpeg', dpi=1000, antialias="none")


########## Keyword only ############
all.manual = all.short %>% 
  filter(Test_Type == 'Validation Keyword Only')

all.manual = all.manual %>% dplyr::select(!Test_Type)

all.manual2 = all.manual %>% pivot_longer(!c(Topic,model),
  names_to = "Measure",
  values_to = "count"
)

all.manual2$Topic = factor(all.manual2$Topic, 
                                     levels = c( 
                                                "Economy","Entertainment","Sports","Politics"))


p1=ggplot(all.manual2, aes(x =count, y=Topic, color=Measure, shape=Measure)) + geom_point(size=3) +
  facet_grid(. ~ model) + theme_bw() + xlab("")+
    theme(legend.position="bottom") + scale_colour_grey(start = 0, end = .7)+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "0.25", "0.5", "0.75", "1.0"))

p1
path = 'keyword_results.jpeg'

ggsave(path, plot = p1, height = 4, width = 7.5, units = "in", device='jpeg', dpi=1000, antialias="none")

########## Distant supervision #######
all.manual = all.short %>% 
  filter(Test_Type == 'Validation')

all.manual = all.manual %>% dplyr::select(!Test_Type)

all.manual2 = all.manual %>% pivot_longer(!c(Topic,model),
  names_to = "Measure",
  values_to = "count"
)

all.manual2$Topic = factor(all.manual2$Topic, 
                                     levels = c( 
                                                "Economy","Entertainment","Sports","Politics"))

p1=ggplot(all.manual2, aes(x =count, y=Topic, color=Measure, shape=Measure)) + geom_point(size=3) +
  facet_grid(. ~ model) + theme_bw() + xlab("")+
    theme(legend.position="bottom") + scale_colour_grey(start = 0, end = .7)+ coord_cartesian(xlim = c(0,1))+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "0.25", "0.5", "0.75", "1.0"))

p1
path = 'distant_validation_results.jpeg'

ggsave(path, plot = p1, height = 4, width = 7.5, units = "in", device='jpeg', dpi=1000, antialias="none")


########## train-test distant supervision
library(tidyverse)
all.manual = all.short %>% 
  filter(Test_Type == 'Train/Test Split')

all.manual = all.manual %>% dplyr::select(!Test_Type)

all.manual2 = all.manual %>% pivot_longer(!c(Topic,model),
  names_to = "Measure",
  values_to = "count"
)

all.manual2$Topic = factor(all.manual2$Topic, 
                                     levels = c( 
                                                "Economy","Entertainment","Sports","Politics"))


p1=ggplot(all.manual2, aes(x =count, y=Topic, color=Measure, shape=Measure)) + geom_point(size=3) +
  facet_grid(. ~ model) + theme_bw() + xlab("")+
    theme(legend.position="bottom") + scale_colour_grey(start = 0, end = .7)+ coord_cartesian(xlim = c(0,1))+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "0.25", "0.5", "0.75", "1.0"))

p1
path = 'train-test_results.jpeg'

ggsave(path, plot = p1, height = 4, width = 7.5, units = "in", device='jpeg', dpi=1000, antialias="none")



########## Plotting all F1 Scores ############
all.short.distant = all.short 

all.short.distant = all.short.distant %>% 
  filter(Test_Type == 'Validation' | Test_Type == 'Validation Keyword Only' | Test_Type == 'Manual Labels Train/Test Split')


all.short.distant = all.short.distant %>%
  mutate(Test_Type = ifelse(Test_Type == 'Validation', 'Distant Supervision',ifelse(
    Test_Type == 'Validation Keyword Only', 'Keyword Only',ifelse(
      Test_Type == 'Manual Labels Train/Test Split', 'Classic Model', Test_Type))))

all.short.distant$Test_Type = factor(all.short.distant$Test_Type, 
                                     levels = c( 
                                                "Keyword Only","Distant Supervision", "Classic Model"))
all.short.distant$Topic = factor(all.short.distant$Topic, 
                                     levels = c( 
                                                "Economy","Entertainment","Sports","Politics"))

#Plotting all results
p1 =ggplot(all.short.distant, aes(x =F1, y=Topic, fill=Test_Type)) +
  geom_bar(stat='identity', position='dodge', color = 'black') + 
  facet_grid(. ~ model) + theme_bw() + 
    theme(legend.position="bottom") +
  scale_fill_grey(start = 0, end = .9,name = "Classification Type")

p1
path = 'all_f1_scores.jpeg'

ggsave(path, plot = p1, height = 4, width = 7.5, units = "in", device='jpeg', dpi=1000, antialias="none")








