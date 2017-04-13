########################

varLots <- lots %>%
  filter(!is.na(value)) %>%
  group_by(year, round, sample, lot, gid) %>%
  mutate(n= n()) %>%
  group_by(round, sample, gid, year) %>%
  filter(max(n) > 10) %>%
  mutate(sd =  getSfromAlgA(value), n_all = n()) %>%
  group_by(round, sample, lot, gid, year) %>%
  summarise(sd_lot = getSfromAlgA(value), 
            sd_lot_e = getStErrorForS(value),
            n_lot = n(),
            sd_all = sd[1], 
            n_all = n_all[1]) %>%
  mutate(perc = sd_lot/sd_all) %>%
  filter(!is.na(perc))

vl <- varLots %>%
  ungroup() %>%
  filter(!is.na(lot))


############################

frequentLots <- lots %>%
  filter(!is.na(value)) %>%
  filter(!is.na(lot)) %>%
  filter(abs(relDiff) < .5) %>%
  group_by(round, sample, lot, gid, year) %>%
  mutate(n_in_lot = n()) %>%
  filter(n_in_lot > 7) %>%
  group_by(round, sample, gid, year) %>%
  filter(n_distinct(lot) > 1)

diffBetweenLots <- ddply(frequentLots, 
                         c("round", "sample", "gid", "year"),  function(x){
                           quantilesFromA(x)
                         })

diffBetweenLots <- diffBetweenLots[order(diffBetweenLots$all, decreasing = F),]
diffBetweenLots$over <- ifelse(diffBetweenLots$all > 0.05, 'o', 'u')

diffBetweenLots$id <- as.factor(1:nrow(diffBetweenLots))

save(file=paste0(base.dir, 'generated/diffsInLots.RData'), diffBetweenLots)

pLots <- ggplot(diffBetweenLots, 
                aes(x=id, y=all, ymin=p025, ymax=p975, fill=over))+ 
  geom_pointrange(shape=21, size=.2)  + 
  coord_flip() +
  scale_fill_manual(values=c('o'= 'black', 'u' = 'white')) +
  scale_y_continuous(labels=percent) +
  theme_Publication(base_size = 10) +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none") +
  xlab('') +
  ylab("maximum difference between lots\n in same EQA round")

ggsave(paste0(base.dir, 'fig/lots.png'),
       pLots,  dpi = 600, width = 85, height= 100, units='mm')
