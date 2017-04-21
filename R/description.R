library(rtf)

descriptionRV <- eqaAll %>%
  group_by(eqa) %>%
  mutate(allParticipants = n_distinct(pid)) %>%
  ungroup() %>%
  select(eqa, year, round, pid, status, allParticipants) %>%
  distinct() %>%
  group_by(eqa, year, round) %>%
  summarise(participants = n_distinct(pid),
            successRate = sum(status != 'fail')/n(), 
            allParticipants = allParticipants[1]) %>%
  group_by(eqa) %>%
  summarise(allParticipants = allParticipants[1],
            minParticipants = min(participants),
            meanParticipants = round(mean(participants)),
            maxParticipants = max(participants),
            minSuccessRate = round(min(successRate), 2),
            meanSuccessRate = round(mean(successRate), 2),
            maxSuccessRate = round(max(successRate), 2))


descriptionSamples <- eqaAll %>% 
  group_by(eqa) %>%
  summarise(samples = n(), 
            minValue = min(target),
            meanValue = round(mean(target), 2),
            maxValue = max(target),
            roundsPerYear = max(round)) %>%
  ungroup() 

descrAll <- rbind(t(descriptionRV), t(descriptionSamples))
colnames(descrAll) <- descrAll[1,]
descrAll <- descrAll[-c(1,9),]
descrAll <- cbind(rownames(descrAll),descrAll)
colnames(descrAll)[1] <- ' '

rtf<-RTF(paste0(base.dir,'tab/description.rtf'))
addTable(rtf,descrAll)
done(rtf)