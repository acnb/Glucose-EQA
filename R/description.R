captions <- c(
  'nPart' = 'number of distinct participants',
  'participants' = 'distinct participants per distribution',
  'successRate' = 'success rate per distribution',
  'nSamples' = 'total number of samples', 
  'rmv' = 'reference method value mg/dL (mmol/L)',
  'nRounds' = 'distributions per year',
  'nDev' = 'number of distinct devices'
)

descriptionRounds <- eqaAll %>%
  select(eqa, year, round, pid, status) %>%
  distinct() %>%
  group_by(eqa, year, round) %>%
  summarise(participants = n_distinct(pid),
            successRate = sum(status != 'failed')/n()) %>%
  gather(key, value, participants, successRate) %>%
  group_by(eqa, key) %>%
  summarise(
    minimum = min(value),
    mean = mean(value),
    maximum = max(value) 
  ) %>%
  ungroup() %>%
  gather(stat, value, minimum, mean, maximum)

descriptionSamples <- eqaAll %>%
  filter(!is.na(rmv)) %>%
  select(eqa, year, round, sample, rmv) %>%
  distinct() %>%
  mutate(key = 'rmv') %>%
  group_by(eqa, key) %>%
  summarise(
    minimum = min(rmv),
    mean = mean(rmv),
    maximum = max(rmv) 
  ) %>%
  ungroup() %>%
  gather(stat, value, minimum, mean, maximum)

descriptionEqa <- eqaAll %>%
  group_by(eqa) %>%
  summarise(
    nPart = n_distinct(pid),
    nDev = n_distinct(device),
    nRounds = max(round),
    nSamples = n()
  ) %>%
  gather(key, value, nPart, nDev, nRounds, nSamples) %>%
  mutate(stat = '')


descriptionAll <- rbind(descriptionEqa, descriptionRounds, descriptionSamples) %>%
  commonOrder() 


descTable <- descriptionAll %>%
  mutate(value = case_when(
    key == 'nPart' ~ as.character(round(value, 0)),
    key == 'nDev' ~ as.character(round(value, 0)),
    key == 'nRounds' ~ as.character(round(value, 0)),
    key == 'nSamples' ~ as.character(round(value, 0)),
    key == 'participants' ~ as.character(round(value, 1)),
    key == 'successRate' ~ as.character(round(value, 2)),
    key == 'rmv' ~ paste0(round(value, 0), ' (', round(value/mmolConvFactor, 2),
                          ')'),
    TRUE ~ as.character(value)
  )) %>%
  spread(eqa, value) %>%
  mutate(stat = as.factor(stat)) %>%
  mutate(stat = fct_relevel(stat, 'minimum', 'mean', 'maximum')) %>%
  arrange(stat) %>%
  group_by(key) %>%
  summarise_all(paste0, collapse = '\n') %>%
  ungroup() %>%
  rowwise() %>%
  ungroup() %>%
  mutate(key = as.factor(key)) %>%
  mutate(key = fct_relevel(key, "nPart", "nSamples", "nDev", "nRounds",
                           "participants", "successRate", "rmv" )) %>%
  arrange(key) %>%
  mutate(key = as.character(key)) %>%
  mutate(key = if_else(key %in% names(captions), captions[key], key))

colnames(descTable)[c(1,2)] <- ' '

rtf<-RTF(here('tab', 'description.rtf'))
addTable(rtf,descTable)
done(rtf)  
