prevYears <- eqaAll %>%
  filter(year < 2016) %>%
  filter(device != "Anderes Gerät" &
           device != "unspecified" &
           device != "others" &
           device != "") %>%
  group_by(type, pid, eqa, year, device) %>%
  mutate(failedDev = ('failed' %in% status)) %>%
  ungroup() %>%
  mutate(isOld = TRUE) %>%
  select(type, pid, eqa, year, device, failedDev, isOld, device) %>%
  unique() %>%
  mutate(year=year+1)


thisYears <- eqaAll %>%
  filter(year > 2012) %>%
  filter(device != "Anderes Gerät" &
           device != "unspecified" &
           device != "others" &
           device != "") %>%
  group_by(pid, eqa, year, device) %>%
  mutate(failedDev = ('failed' %in% status)) %>%
  ungroup() %>%
  mutate(isOld = FALSE) %>%
  select(pid, eqa, year, device, failedDev, isOld, device, type) %>%
  unique()

devChanges <- rbind(prevYears, thisYears) %>%
  group_by(eqa, pid, year) %>%
  filter(any(isOld)) %>%
  mutate(pHasFailed = any(failedDev[isOld]),
         outcome = case_when(
           all(isOld) ~ 'left',
           any(failedDev[!isOld]) ~ 'failed',
           TRUE ~ 'notFailed')) %>%
  group_by(type, eqa, pid, year, device, outcome, pHasFailed) %>% 
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
  select(type, eqa, pid, year, act, pHasFailed) %>%
  unique() %>%
  group_by(type, eqa, act, pHasFailed) %>%
  summarise(n = n()) %>%
  group_by(eqa, pHasFailed) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  commonOrder() %>%
  mutate(eqa = as.character(eqa)) %>%
  mutate(eqa = if_else(str_length(eqa) > 7,
                       str_replace(eqa, '-', "-\n"),
                       eqa)) 

outcomeFailedAgain <- devChanges %>%
  filter(pHasFailed) %>%
  filter(outcome != 'left') %>%
  select(type, eqa, pid, year, outcome) %>%
  unique() %>%
  group_by(type,eqa, outcome) %>%
  summarise(n = n()) %>%
  group_by(eqa) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  commonOrder() %>%
  mutate(eqa = as.character(eqa)) %>%
  mutate(eqa = if_else(str_length(eqa) > 7, 
         str_replace(eqa, '-', "-\n"), 
         eqa)) 


ggplot(outcomeStats, aes(x=act, y=p, label = n, fill =  pHasFailed)) +
  geom_rect(data = outcomeStats, aes(fill = type),
            xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_col(position = 'dodge')+
  geom_text(position = position_dodge(width = 1), aes(y=p+0.05),  size=2.5)+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  scale_x_discrete(labels = c(
    'left' = 'left EQA',
    'newDev'= 'new device',
    'noNewDev' = "no new device"
  )) +
  scale_fill_manual(
    values = c('TRUE' = '#d7191c', 'FALSE' = '#71A20A', typeColors),
    labels = c('TRUE' ='after year with failures',
               'FALSE' = 'after year without failures'),
    breaks = c('TRUE', 'FALSE')) +
  facet_grid(.~eqa)+ 
  xlab("pathway of EQA participants") +
  ylab('participants') +  
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())

ggpub("pathAll", height = 120)

changeGraph <- eqaAll %>%
  filter(type == 'POCT') %>%
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
  filter(type == 'POCT') %>%
  inner_join(devChanges, by=c("pid" = "pid", "year" = "year", 
                              'eqa' = 'eqa', 'device'='device')) %>%
  filter(pHasFailed) %>%
  group_by(eqa, devNotFailedBefore) %>%
  summarise(n=n_distinct(id)) %>%
  group_by(eqa) %>%
  mutate(p=n/sum(n)) %>%
  ungroup() %>%
  mutate(label = paste0(n, "\n[", round(p,2)*100, "%]", sep=""))

ggplot() + 
  geom_col(data = changeGraph, aes(x=devNotFailedBefore,  
                                   y=p, fill=status),
           position = position_stack(reverse = TRUE)) +
  geom_text(data = changeGraphN, aes(x=devNotFailedBefore, 
                                     y=1.1, label=label),size=3)+ 
  scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1),
                     limits = c(0, 1.15)) +
  scale_x_discrete(labels = c('TRUE' = 'new device', 
                              'FALSE' = 'no new device')) + 
  scale_fill_manual(values=colors.status) +
  xlab('performance after year\n with failed EQA') +
  ylab('individual EQA participations') +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank()) +
  facet_grid(~eqa)

ggpub("pathDetail", width = 75, height = 120)
