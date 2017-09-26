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
                              !(eqa == 'Instand 800' & year == '2015' & 
                                  round == '4' & sample == '2' & split == '30') & 
                              !(eqa == 'Instand 800' & year == '2012' &
                                  round == '3' & sample == '2' & split == '90') &
                              !(eqa == 'Instand 800' & year == '2012' &
                                  round == '3' & sample == '2' & split == '60')) 
                           ), 'failed',
                         ifelse(
                          max((abs(value-target)-2)/target) > .1,
                           'poor',
                          ifelse(
                           max((abs(value-target)-2)/target) > .05,
                             'acceptable', 'good')))) %>%
  ungroup() %>%
  mutate(split = ifelse(eqa=='Instand 800' & year == '2011' & 
                          round == 2 & split %in% c('83', '84'), 
         '83+84', split)) %>%
  
  mutate(eqaRound = paste0(eqa, '-', year, '-', round)) %>%
  left_join(sharedDevs, by=c('device' = 'device', 'eqa' = 'eqa')) %>%
  # mutate(sharedDevice = ifelse(eqa == 'RfB GL' & !is.na(devInstand),
  #                              devInstand, NA)) %>%
  # mutate(sharedDevice = ifelse(eqa == 'Instand 800' & 
  #                                device %in% sharedDevs$devInstand,
  #         device, sharedDevice)) %>%
  # mutate(sharedDevice = ifelse(eqa == 'RfB KS', NA, sharedDevice)) %>%
  # mutate(sharedDevice = ifelse(eqa == 'RfB GL' & split == 'Anderes Gerät', 
  #                              'others', sharedDevice)) %>%
  # mutate(sharedDevice = ifelse(eqa == 'Instand 800' & split == '90', 
  #                              'others', sharedDevice)) %>%
  mutate(status = factor(status, 
                         levels=c('failed', 'poor', 'acceptable', 'good'),
                         ordered = TRUE),
         eqa = factor(eqa),
         device = factor(device),
         meth = factor(meth),
         type = if_else(eqa == 'RfB KS' | eqa == 'Instand 100',
                        'CL', 'POCT'),
         sharedDevice = factor(sharedDevice),
         eqaRound = factor(eqaRound),
         year = as.numeric(year),
         round = as.numeric(round)) 
