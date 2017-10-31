statsBySample <- eqaAll %>% 
#  mutate(useForMean = (year == '2016' | abs(relDiff) < .5)) %>%
  group_by(eqa, year, round, split, sample) %>%
  mutate( n = n()) %>%
  ungroup() %>%
  filter( year == '2016' | abs(relDiff) < 1) %>%
  group_by(eqa, year, round, split, sample) %>%
  summarise(n=n[1], mean = mean(value, na.rm=T), 
            median=median(value, na.rm=T), target=target[1],
            p = sum(abs(relDiff) <= .15)/n())

statsByRound.Split <- eqaAll %>% 
  group_by(eqa, year, round, split, pid) %>%
  summarise(status = min(status)) %>%
  group_by(eqa, year, round, split) %>%
  summarise(n = n(), p = sum(status != 'failed')/n())

statsByRound.All <- eqaAll %>% 
  group_by(eqa, year, round, pid) %>%
  summarise(status = min(status)) %>%
  group_by(eqa, year, round) %>%
  summarise(n = n(), p = sum(status != 'failed')/n())



for(e in unique(eqaAll$eqa)){
  rtf<-RTF(here::here('tab', paste0(e, ' - statsBySample.rtf')))
  addTable(rtf,statsBySample %>% filter(eqa == e))
  done(rtf)
  
  rtf<-RTF(here::here('tab', paste0( e, ' - statsByRound.All.rtf')))
  addTable(rtf,statsByRound.All %>% filter(eqa == e))
  done(rtf)
  
  rtf<-RTF(here::here('tab', paste0( e, ' - statsByRound.Split.rtf')))
  addTable(rtf,statsByRound.Split %>% filter(eqa == e))
  done(rtf)
}


# compare instand 100 with web ----

library(jsonlite)
library(httr)
library(stringr)

numFromHtml <- function(h){
  as.numeric(str_match(h, ">(.+)<")[,2])
}



instand100 <- data.frame()

for(y in 2012:2016){
  url <- paste0('https://www.instand-ev.de/System/php/ajax.auswertung.php?',
                'L=en&rvid=-', y, '&grp=100&best=GLUC&json=1')
  
  a <- GET(url)
  a <- content(a, as="text")
  df <- fromJSON(a, simplifyDataFrame = TRUE)
  
  dfx <- df$data
  
  eqa <- 'CL-Instand'
  year <- NULL
  round <- NULL
  for(i in 1:nrow(dfx)){
    cdf <- dfx[i,]
    if (cdf$type == 'title-row'){
      year <- str_sub(cdf$cells[[1]], 4, 7)
      round <- str_trim(str_sub(cdf$cells[[1]], 8, 10))
    }
    else{
      cell <- cdf$cells[[1]]
      tmp <- data.frame(eqa = eqa, year = year, round = round, grpStr = cell[1],
                        sample = str_sub(cell[2], 2,2), zw = numFromHtml(cell[3]),
                        mittelwert = numFromHtml(cell[5]), n = numFromHtml(cell[7]),
                        q = numFromHtml(cell[8]))
      instand100 <- rbind(instand100, tmp)
    }
  }
}

instand100 <- instand100 %>%
  mutate_at(vars(eqa, year, sample, round), as.character) %>%
  mutate_at(vars(round, year), as.numeric)

comp100 <- instand100 %>%
  left_join(statsBySample, by=c('eqa' = 'eqa', 
                                'year' = 'year', 
                                'round' = 'round',
                                'sample' = 'sample')) %>%
  mutate(diffN = n.x-n.y, diffMean = mittelwert - mean, qDiff = q-(p*100))

# compare instand 800 with web ----

instand800 <- data.frame()

for(y in 2012:2016){
  url <- paste0('https://www.instand-ev.de/System/php/ajax.auswertung.php?',
                'L=en&rvid=-', y, '&grp=800&best=GLUC&json=1')
  
  a <- GET(url)
  a <- content(a, as="text")
  df <- fromJSON(a, simplifyDataFrame = TRUE)
  
  dfx <- df$data
  
  eqa <- 'Instand 800'
  year <- NULL
  round <- NULL
  for(i in 1:nrow(dfx)){
    cdf <- dfx[i,]
    if (cdf$type == 'title-row'){
      year <- str_sub(cdf$cells[[1]], 4, 7)
      round <- str_trim(str_sub(cdf$cells[[1]], 8, 10))
    }
    else{
      cell <- cdf$cells[[1]]
      tmp <- data.frame(eqa = eqa, year = year, round = round, grpStr = cell[1],
                        sample = str_sub(cell[2], 2,2), zw = numFromHtml(cell[3]),
                        mittelwert = numFromHtml(cell[5]), n = numFromHtml(cell[7]),
                        q = numFromHtml(cell[8]))
      instand800 <- rbind(instand800, tmp)
    }
  }
}

instand800 <- instand800 %>%
  mutate_at(vars(eqa, year, sample, round), as.character) %>%
  mutate_at(vars(round, year), as.numeric)

samples <- instand800 %>%
  select(year, round, sample) %>%
  unique()

comp800 <- data.frame()

for(i in 1:nrow(samples)){
  s <- samples[i,]
  web <- instand800 %>%
    filter(year == s$year & round == s$round & sample == s$sample) %>%
    arrange(zw, n)
  
  data <- statsBySample %>%
    filter(eqa == 'Instand 800' & year == s$year &
             round == s$round & sample == s$sample) %>%
    arrange(target, n)
  
  web$n.data <- data$n 
  web$mean.data <- data$mean
  web$target <- data$target
  web$q.data <- data$p*100
  web$diff.q <- web$q - data$p*100
  web$split <- data$split
  
  comp800 <- rbind(comp800, web)
  }

comp800$diff.mean <- comp800$mittelwert - comp800$mean.data
comp800$diff.n <- comp800$n - comp800$n.data

selSample <- eqaAll %>%
  filter(eqa == 'Instand 800' & year == '2013' & round == '3' & sample == '2' &
           split == '17') %>%
  filter(abs(relDiff) < 1)
mean(selSample$value)
