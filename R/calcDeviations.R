eqaAll <- eqaAll %>%
  group_by(id) %>%
  filter(n() == 2) %>%
  mutate(relDiff = (value-target)/target) %>%
  mutate(status = ifelse(max(abs(relDiff)) > .15 | 
                           NA %in% relDiff, 'fail',
                         ifelse(max(abs(relDiff)) > .1, 'poor', 
                                'good'))) %>%
  ungroup() %>%
  mutate(status = factor(status, levels=c('fail', 'poor', 'good')),
         year = as.numeric(year),
         round = as.numeric(round)) 


lots <- lots %>%
  group_by(id) %>%
  filter(n() == 2) %>%
  mutate(relDiff = (value-target)/target) %>%
  mutate(status = ifelse(max(abs(relDiff)) > .15 | 
                           NA %in% relDiff, 'fail',
                         ifelse(max(abs(relDiff)) > .1, 'poor', 
                                'good'))) %>%
  ungroup() %>%
  mutate(status = factor(status, levels=c('fail', 'poor', 'good')),
         year = as.numeric(year),
         round = as.numeric(round)) 
