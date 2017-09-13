frequentLots <- eqaAll %>%
  filter(!is.na(value)) %>%
  filter(!is.na(lot)) %>%
  mutate(lot = factor(lot)) %>%
  filter(abs(relDiff) < .45) %>%
  group_by(round, sample, lot, device, year) %>%
  mutate(n_in_lot = n()) %>%
  filter(n_in_lot > 7) %>%
  mutate(e = 1.253*(getSfromAlgA(value)/getMufromAlgA(value))/sqrt(n_in_lot)) %>%
  filter(e < .025) %>%
  group_by(round, sample, device, year) %>%
  filter(n_distinct(lot) > 1) %>%
  ungroup()

diffBetweenLots <- ddply(frequentLots, 
                         c("round", "sample", "device", "year"),  function(x){
                           quantilesFromA(x)
                         })

diffBetweenLots <- diffBetweenLots[order(diffBetweenLots$all, decreasing = F),]
diffBetweenLots$over <- ifelse(diffBetweenLots$all > 0.05, 'o', 'u')

diffBetweenLots$id <- as.factor(1:nrow(diffBetweenLots))

save(file=here('generated', 'diffsInLots.RData'), diffBetweenLots)

pLots <- ggplot(diffBetweenLots, 
                aes(x=id, y=all, ymin=p025, ymax=p975, fill=over))+ 
  geom_pointrange(shape=21, size=.2)  + 
  coord_flip() +
  scale_fill_manual(values=c('o'= 'black', 'u' = 'white')) +
  scale_y_continuous(labels=percent) +
  theme_pub(base_size = 10) +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none") +
  xlab('') +
  ylab("maximum difference between lots\n in same EQA round")

ggpub('lots', width = 85, height= 100)
