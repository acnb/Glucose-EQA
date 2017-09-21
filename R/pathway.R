prevYears <- eqaAll %>%
  filter(year < 2016) %>%
  filter(device != "Anderes Gerät" &
           device != "unspecified" &
           device != "others" &
           device != "") %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL' | eqa == 'RfB KS') %>%
  group_by(pid, eqa, year, device) %>%
  mutate(failedDev = ('failed' %in% status)) %>%
  ungroup() %>%
  mutate(isOld = TRUE) %>%
  select(pid, eqa, year, device, failedDev, isOld, device) %>%
  unique() %>%
  mutate(year=year+1)


thisYears <- eqaAll %>%
  filter(year > 2012) %>%
  filter(device != "Anderes Gerät" &
           device != "unspecified" &
           device != "others" &
           device != "") %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL' | eqa == 'RfB KS') %>%
  group_by(pid, eqa, year, device) %>%
  mutate(failedDev = ('failed' %in% status)) %>%
  ungroup() %>%
  mutate(isOld = FALSE) %>%
  select(pid, eqa, year, device, failedDev, isOld, device) %>%
  unique()

devChanges <- rbind(prevYears, thisYears) %>%
  group_by(eqa, pid, year) %>%
  filter(any(isOld)) %>%
  mutate(pHasFailed = any(failedDev[isOld]),
         outcome = case_when(
           all(isOld) ~ 'left',
           any(failedDev[!isOld]) ~ 'failed',
           TRUE ~ 'notFailed')) %>%
  group_by(eqa, pid, year, device, outcome, pHasFailed) %>% 
  summarise(new = !any(isOld), 
            devNotFailedBefore = case_when(!any(isOld) ~ TRUE,
                                           all(!failedDev[isOld]) ~ TRUE,
                                           TRUE ~ FALSE)) %>%
  group_by(eqa, pid, year) %>%
  mutate(act = case_when(
    outcome == 'left' ~ 'left',
    length(device[new]) > 0 ~ 'newDev',
    TRUE ~ 'noNewDev')) %>%
  ungroup()


outcomeStats <- devChanges %>%
  select(eqa, pid, year, act, pHasFailed) %>%
  unique() %>%
  group_by(eqa, act, pHasFailed) %>%
  summarise(n = n()) %>%
  group_by(eqa, pHasFailed) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  commonOrder()

outcomeFailedAgain <- devChanges %>%
  filter(pHasFailed) %>%
  filter(outcome != 'left') %>%
  select(eqa, pid, year, outcome) %>%
  unique() %>%
  group_by(eqa, outcome) %>%
  summarise(n = n()) %>%
  group_by(eqa) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  commonOrder()

print(outcomeFailedAgain)


ggplot(outcomeStats, aes(x=act, y=p, label = n, fill =  pHasFailed)) +
  geom_col(position = 'dodge')+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  scale_x_discrete(labels = c(
    'left' = 'left EQA',
    'newDev'= 'new device',
    'noNewDev' = "no new device"
  )) +
  scale_fill_manual(
    values = c('TRUE' = '#d7191c', 'FALSE' = '#71A20A'),
    labels = c('TRUE' ='after year with failures',
               'FALSE' = 'after year without faiures')) +
  facet_grid(.~eqa)+ 
  xlab("yearly actions in EQAs") +
  ylab('percentage of participants in each year') +  
  # geom_text(
  #   aes(label = n, y = p + 0.02),
  #   position = position_dodge(0.9),
  #   vjust = 0) +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())

ggpub("pathAll", width = 100, height = 120)

changeGraph <- eqaAll %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL') %>%
  inner_join(devChanges ,
             by=c("pid" = "pid", 
                  "year" = "year",
                  'eqa' = 'eqa',
                  'device'='device')) %>%
  filter(pHasFailed) %>%
  group_by(eqa, devNotFailedBefore, status) %>%
  summarise(n=n_distinct(id)) %>%
  mutate(p=n/sum(n)) %>%
  ungroup() %>%
  commonOrder()

changeGraphN <- eqaAll %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL') %>%
  inner_join(devChanges, by=c("pid" = "pid", "year" = "year", 
                              'eqa' = 'eqa', 'device'='device')) %>%
  filter(pHasFailed) %>%
  group_by(eqa, devNotFailedBefore) %>%
  summarise(n=n_distinct(id)) %>%
  ungroup()

ggplot() + 
  geom_col(data = changeGraph, aes(x=devNotFailedBefore,  
                                   y=p, fill=status),
           position = position_stack(reverse = TRUE)) +
  geom_text(data = changeGraphN, aes(x=devNotFailedBefore, 
                                     y=1.1, label=n),size=3)+ 
  scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
  scale_x_discrete(labels = c('TRUE' = 'new device', 
                              'FALSE' = 'no new device')) + 
  scale_fill_manual(values=colors.status) +
  xlab('performance after year\n with failed EQA') +
  ylab('percentage of EQA participations') +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank()) +
  facet_grid(~eqa)

ggpub("pathDetail", width = 75, height = 120)
