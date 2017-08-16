bias <- eqaAll %>%
  dplyr::filter(eqa == 'RfB KS') %>%
  dplyr::filter(abs(relDiff) < .45) %>%
  dplyr::group_by(device, year, round, sample) %>%
  dplyr::filter(n() > 7) %>%
  dplyr::summarise(n = n(), 
          rmv = rmv[1], 
          stableMu = getMufromAlgA(value)
          ) %>%
  ungroup() %>%
  dplyr::mutate(diff = rmv - stableMu, diff.rel = (rmv - stableMu)/rmv)

biasByRoundAbs <- bias %>%
  group_by(device, year, round) %>%
  summarise(max = max(diff), rmv = rmv[diff == max(diff)], n = sum(n))


biasByDevice <- bias %>%
  group_by(device) %>%
  summarise(m = mean(diff), s = sd(diff), n = sum(n)) %>%
  ungroup()
  