statsRmwYear <- eqaAll %>%
  filter(eqa == 'POCT-Instand') %>%
  filter(!is.na(rmv)) %>%
  group_by(year, split, round) %>%
  summarise(hasRmv = all(target == rmv)) %>%
  group_by(year) %>%
  summarise(p = sum(hasRmv)/n(), nHasRmw = sum(hasRmv), n = n())

statsRmwYearRound <- eqaAll %>%
  filter(eqa == 'POCT-Instand') %>%
  filter(!is.na(rmv)) %>%
  group_by(year, round, split) %>%
  summarise(hasRmv = all(target == rmv)) %>%
  group_by(year, round) %>%
  summarise(p = sum(hasRmv)/n(), nHasRmw = sum(hasRmv), n = n())

ggplot()+
  geom_point(data = statsRmwYear, aes(x=3.5, y = p, shape = 'g'), size = 3)+
  geom_point(data = statsRmwYearRound, aes(x=round, y = p, shape = 'a')) +
  scale_y_continuous(labels= percent) +
  scale_x_continuous(breaks = 1:6) +
  scale_shape_discrete(labels = c('a' = 'by single distribution',
                                  'g' = 'by year')) +
  facet_grid(.~year)+
  xlab('') + 
  ylab('fraction of subgroubs evaluated\nusing reference method values') +
  theme_pub() +
  theme(legend.title = element_blank())

ggpub('rmvEval', height = 120)
