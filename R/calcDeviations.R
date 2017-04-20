# Instand 100, 2015, 3: n = 1
# Instand 111, 2015, 3: n = 1
# Instand 111, 2012, 5: ?
# Instand 111, 2015, 4: n = 1


eqaAll <- eqaAll %>%
  filter(!is.na(value)) %>%
  filter(value > 0) %>%
  group_by(id) %>%
  mutate(relDiff = (value-target)/target) %>%
  mutate(status = ifelse(max(abs(relDiff)) > .15 |  ((n() < 2) & (
                              !(eqa == 'Instand 100' & year == '2015' & 
                                  round == '3') &
                              !(eqa == 'Instand 111' & year == '2015' &
                                  round == '3' & sample == '1') &
                              !(eqa == 'Instand 111' & year == '2015' &
                                  round == '4' & sample == '1') &
                              !(eqa == 'Instand 800' & year == '2011' &
                                  round == '3' & sample == '2' & split == '76') &
                              !(eqa == 'Instand 800' & year == '2011' &
                                  round == '3' & sample == '1' & split == '74') &
                              !(eqa == 'Instand 800' & year == '2015' &
                                  round == '4' & sample == '2' & split == '30') &
                              !(eqa == 'Instand 800' & year == '2011' &
                                  round == '6' & sample == '2' & split == '80') &
                              !(eqa == 'Instand 800' & year == '2011' &
                                  round == '6' & sample == '2' & split == '76') &
                              !(eqa == 'Instand 800' & year == '2012' &
                                  round == '3' & sample == '2' & split == '90') &
                              !(eqa == 'Instand 800' & year == '2012' &
                                  round == '3' & sample == '2' & split == '60')) 
                           ), 'fail',
                         ifelse(max(abs(relDiff)) > .1, 'poor', 
                                'good'))) %>%
  ungroup() %>%
  mutate(eqaRound = paste0(eqa, '-', year, '-', round)) %>%
  mutate(status = factor(status, levels=c('fail', 'poor', 'good'), ordered = TRUE),
         eqa = factor(eqa),
         device = factor(device),
         eqaRound = factor(eqaRound),
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
