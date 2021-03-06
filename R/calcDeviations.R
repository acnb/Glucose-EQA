# Instand 100, 2015, 3: n = 1

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
  mutate_at(vars(device, eqa), as.character) %>%
  left_join(sharedDevs, by=c('device' = 'device', 'eqa' = 'eqa')) %>%
  mutate(eqa = case_when(eqa == 'Instand 100' ~ 'CL-Instand',
                         eqa == 'Instand 800' ~ 'POCT-Instand',
                         eqa == 'RfB KS' ~ 'CL-RfB',
                         eqa == 'RfB GL' ~ 'POCT-RfB',
                         TRUE ~ NA_character_)) %>%
  mutate(status = factor(status, 
                         levels=c('failed', 'poor', 'acceptable', 'good'),
                         ordered = TRUE),
         eqa = factor(eqa),
         device = factor(device),
         meth = factor(meth),
         type = if_else(eqa == 'CL-Instand' | eqa == 'CL-RfB',
                        'CL', 'POCT'),
         eqaRound = factor(eqaRound),
         year = as.numeric(year),
         round = as.numeric(round)) %>%
  mutate(meth = fct_collapse(meth, 
                             "Glucose oxidase" = c("GOD-PAP",
                                                   "other GOD/POD methods",
                                                   "GOD-PERID",
                                                   "Glucose oxidase/H2O2-electrode",
                                                   "Glucose oxidase/PAP"),
                             "Glucose-6-phosphate dehydrogenase" = c(
                               "GLUC-DH (UV-test)"))) %>%
  mutate(sharedDevice = case_when(
    sharedDevice == "Beckman Coulter other devices" & 
      meth == "Glucose oxidase" ~ "Beckman Coulter other devices [GO]",
    sharedDevice == "Beckman Coulter other devices" & 
      meth == "Hexokinase" ~ "Beckman Coulter other devices [HK]",
    sharedDevice == "Beckman Coulter other devices" ~
      "Beckman Coulter other devices [others]",
    sharedDevice == "Roche Diagnostics" & 
      meth == "Glucose oxidase" ~ "Roche Diagnostics [GO]",
    sharedDevice == "Roche Diagnostics" & 
      meth == "Hexokinase" ~ "Roche Diagnostics [HK]",
    sharedDevice == "Roche Diagnostics" ~
      "Roche Diagnostics [others]",
    TRUE ~ sharedDevice
  )) 

# remove rarely used
eqaAll <- eqaAll %>%
  group_by(sharedDevice, eqa) %>%
  mutate(nlabs = n_distinct(pid), nRounds = n_distinct(eqaRound)) %>%
  group_by(sharedDevice) %>%
  mutate(minLabs = min(nlabs), minRounds = min(nRounds), n = n(), nEqa = n_distinct(eqa), n = n()) %>%
  ungroup() %>%
  mutate(sharedDevice = if_else(
    nEqa < 2 | minLabs < 10 | minRounds < 3 | n < 100,
    "others", sharedDevice)) %>%
  mutate(sharedDevice = if_else(is.na(sharedDevice),
    "others", sharedDevice)) %>%
  mutate(sharedDevice = factor(sharedDevice)) %>%
  mutate(minLabs = NULL, minRounds = NULL, n = NULL, nEqa = NULL, n = NULL)
      
      
      


  
