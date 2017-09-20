bias <- eqaAll %>%
  dplyr::filter(eqa == 'RfB KS' | eqa == 'Instand 800') %>% 
  dplyr::filter(target == rmv) %>%
  dplyr::filter(device != 'others') %>%
  dplyr::filter(device != 'unspecified') %>%
  dplyr::filter(abs(relDiff) < .45) %>%
  dplyr::group_by(eqa, device, year, round, sample) %>%
  dplyr::filter(n() > 7) %>%
  dplyr::summarise(n = n(), 
          rmv = rmv[1], 
          stableMu = getMufromAlgA(value)
          ) %>%
  rowwise() %>%
  dplyr::mutate(device = replaceCLNames(as.character(device))) %>%
  ungroup() %>%
  dplyr::mutate(device = str_replace(device, "\n", " ")) %>%
  dplyr::mutate(bias.abs = rmv - stableMu, bias.rel = (rmv - stableMu)/rmv)


ggplot(bias, aes(x=device, y=bias.rel)) +
  geom_boxplot()+
  facet_grid(eqa~., scale = 'free_y') +
  theme_pub()+
  scale_y_continuous(labels=percent) +
  xlab('') +
  coord_flip()+
  ylab('relative bias') +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
ggpub("bias", height = 120)

medBias <- bias %>% 
  group_by(device, eqa) %>% 
  summarise(m = median (bias.rel))

print(medBias)
