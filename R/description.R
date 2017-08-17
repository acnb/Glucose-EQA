captions <- list(
  'allParticipants' = 'number of distinct participants',
  'minParticipants' = 'minimum number of participants per round',
  'meanParticipants' = 'mean number of participants per round',
  'maxParticipants' = 'maximum number of participants per round',
  'minSuccessRate' = 'minimum success rate per round',
  'meanSuccessRate' = 'mean success rate per round',
  'maxSuccessRate' = 'maximum success rate per round',
  'samples' = 'total number of samples', 
  'minValue' = 'minimum target value',
  'meanValue' = 'mean target value',
  'maxValue' = 'maximum target value',
  'roundsPerYear' = 'rounds per year',
  'nDevices' = 'number of different devices'
)

descriptionRV <- eqaAll %>%
  group_by(eqa) %>%
  mutate(allParticipants = n_distinct(pid)) %>%
  ungroup() %>%
  select(eqa, year, round, pid, status, allParticipants) %>%
  distinct() %>%
  group_by(eqa, year, round) %>%
  summarise(participants = n_distinct(pid),
            successRate = sum(status != 'failed')/n(), 
            allParticipants = allParticipants[1]) %>%
  group_by(eqa) %>%
  summarise(allParticipants = allParticipants[1],
            minParticipants = min(participants),
            meanParticipants = round(mean(participants)),
            maxParticipants = max(participants),
            minSuccessRate = round(min(successRate), 2),
            meanSuccessRate = round(mean(successRate), 2),
            maxSuccessRate = round(max(successRate), 2))  %>%
  ungroup() %>%
  commonOrder() %>%
  arrange(eqa)


descriptionSamples <- eqaAll %>% 
  group_by(eqa) %>%
  summarise(samples = n(), 
            minValue = min(target),
            meanValue = round(mean(target), 2),
            maxValue = max(target),
            roundsPerYear = max(round),
            nDevices = as.character(n_distinct(device))) %>%
  ungroup() %>%
  mutate(nDevices = ifelse(eqa == 'Instand 100', '-', nDevices)) %>%
  commonOrder() %>%
  arrange(eqa) 
  

descrAll <- rbind(t(descriptionRV), t(descriptionSamples))
colnames(descrAll) <- descrAll[1,]
descrAll <- descrAll[-c(1,9),]
descrAll <- cbind(rownames(descrAll),descrAll)
descrAll <- descrAll %>%
  as.data.frame() %>%
  mutate(V1 = as.character(V1)) %>%
  rowwise() %>%
  mutate(V1 = ifelse(V1 %in% names(captions), captions[[V1]], V1)) %>%
  ungroup() 

colnames(descrAll)[1] <- ' '

rtf<-RTF(here('tab', 'description.rtf'))
addTable(rtf,descrAll)
done(rtf)