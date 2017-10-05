bias <- eqaAll %>%
  dplyr::filter(eqa != 'POCT-RfB') %>% 
  dplyr::filter(!str_detect(device, "Andere")) %>%
  dplyr::filter(!str_detect(device, "andere")) %>%
  mutate(charDev = ifelse(is.na(sharedDevice), 
                          as.character(device),
                          as.character(sharedDevice))) %>%
  filter(charDev != 'Other devices') %>% 
  filter(abs(relDiff) < .45) %>%
  dplyr::filter(charDev != 'others') %>%
  dplyr::filter(target == rmv) %>%
  dplyr::filter(abs(relDiff) < .45) %>%
  dplyr::group_by(type,eqa, charDev, year, round, sample) %>%
  dplyr::filter(n() > 7) %>%
  dplyr::summarise(n = n(), 
          rmv = rmv[1], 
          stableMu = getMufromAlgA(value)
          ) %>%
  dplyr::mutate(bias.abs = rmv - stableMu, bias.rel = (rmv - stableMu)/rmv)

medBias <- bias %>%
  group_by(type, eqa, charDev) %>%
  summarise(medBias = median(bias.rel))


ggplot() +
  geom_rect(data = bias, aes(fill = type),
            xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_boxplot(data = bias, aes(x=charDev, y=bias.rel))+
  facet_grid(eqa~., scale = 'free_y') +
  theme_pub()+
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values= typeColors, guide = "none") +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab('') +
  coord_flip()+
  ylab('relative bias') +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
ggpub("bias", height = 150)

medBias <- bias %>% 
  group_by(type, charDev, eqa) %>% 
  summarise(m = median (bias.rel))

print(medBias)
