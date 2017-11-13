library(readxl)
library(xml2)
library(XML)
library(httr)
library(jsonlite)
library(rvest)


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


### compare RfB to web

zielwerte.rfb <- ldply(c(2012:2016), function(year){
  zw.by.year <- ldply(c(1:4), function(r){
    url <- paste0("http://www.rfb.bio/cgi/displayAnaStats?rv_type=GL&",
                  "rvTypeForDetails=GL&year=", year,
                  "&rv_num=",r ,"&analyte=all&",
                  "searchType=rv_type&anaV=2")
    durl <- gsub('[^a-zA-Z0-9]', '', url)
    if (!file.exists(paste0(here('generated'), '/',  durl, '.html'))){
      print(paste("download", year, r))
      download.file(url, destfile = paste0(here('generated'), '/', durl, 
                                           '.html'))
    }
    base.Page <- read_html(paste0(here('generated'), '/', durl, '.html'), 
                           encoding = "UTF-8")
    
    stat.a.tr <- base.Page %>%
      html_node('table.stats')%>%
      html_node('tbody') %>%
      html_nodes('tr')
    
    min.a <- stat.a.tr %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()
    
    med.a <- stat.a.tr %>%
      html_nodes('td:nth-child(8)') %>%
      html_text()
    
    link.a <- stat.a.tr %>%
      html_nodes('td:nth-child(3)') %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      str_replace("javascript:open_plotwindow\\('", "http://www.rfb.bio/") %>%
      str_replace("'\\)", "")
    
    max.a <- stat.a.tr %>%
      html_nodes('td:nth-child(10)') %>%
      html_text()
    
    stat.b.tr <- base.Page %>%
      html_nodes('table.stats')%>%
      magrittr::extract(2) %>%
      html_node('tbody') %>%
      html_nodes('tr')
    
    min.b <- stat.b.tr %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()
    
    med.b <- stat.b.tr %>%
      html_nodes('td:nth-child(8)') %>%
      html_text()
    
    max.b <- stat.b.tr %>%
      html_nodes('td:nth-child(10)') %>%
      html_text()
    
    link.b <- stat.b.tr %>%
      html_nodes('td:nth-child(3)') %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      str_replace("javascript:open_plotwindow\\('", "http://www.rfb.bio/") %>%
      str_replace("'\\)", "")
    
    stats <- rbind(data.frame('sample' = 'a', min = min.a, med = med.a, max = max.a, 
                              link = link.a),
                   data.frame('sample' = 'b', min = min.b, med = med.b, max = max.b, 
                              link = link.b))%>%
      mutate_at(vars(min, med, max), as.character) %>%
      mutate_at(vars(min, med, max), str_replace, pattern = ',', replacement ='.') %>%
      mutate_at(vars(min, med, max), as.numeric)
    
    
    js.links <- base.Page %>% 
      html_nodes('a.zeile_verlinkung') %>% 
      html_attr('href')
    
    js.links <- js.links[grepl('showPlot', js.links)]
    
    
    js.links <- gsub("javascript:open_plotwindow\\('", "http://www.rfb.bio/", js.links)
    
    js.links <- gsub("'\\)", "", js.links)
    
    zielwerte.einzeln <- data.frame()
    
    for (l in js.links){
      l.durl <- gsub('[^a-zA-Z0-9]', '', l)
      if (!file.exists(paste0(here('generated'), '/',  l.durl, '.html'))){
        print(paste("download", l))
        download.file(l, destfile = paste0(here('generated'), '/', l.durl, 
                                           '.html'))
      }
      plot.page <- read_html(paste0(here('generated'), '/', l.durl, '.html'), 
                             encoding = "UTF-8")
      
      geraet.name <- plot.page %>% 
        html_node("table > tr:nth-child(4) > td") %>% 
        html_text()
      
      zw.a <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(2) > td:nth-child(2)') %>%
        html_text()
      
      zw.b <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(2) > td:nth-child(3)') %>%
        html_text()
      
      
      n <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(1) > td:nth-child(2)') %>%
        html_text()
      
      sd.a <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(5) > td:nth-child(2)') %>%
        html_text()
      
      sd.b <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(5) > td:nth-child(3)') %>%
        html_text()
      
      m.a <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(4) > td:nth-child(2)') %>%
        html_text()
      
      m.b <- plot.page %>%
        html_node('#kitDetails > tr:nth-child(4) > td:nth-child(3)') %>%
        html_text()
      
      if (l %in% stats$link){
        s <- stats %>%
          filter(link == l) %>%
          transmute(min.a = min[sample == 'a'],
                    med.a = med[sample == 'a'],
                    max.a = max[sample == 'a'],
                    min.b = min[sample == 'b'],
                    med.b = med[sample == 'b'],
                    max.b = max[sample == 'b']) %>%
          unique()
      }
      else {
        s <- data.frame(min.a = NA,
                        med.a = NA,
                        max.a = NA,
                        min.b = NA,
                        med.b = NA,
                        max.b = NA)
      }
      
      zielwerte.einzeln <- rbind(zielwerte.einzeln, 
                                 cbind(data.frame("geraet.name" = geraet.name, 
                                                  "zw.a" = zw.a, 
                                                  "zw.b" = zw.b,
                                                  'sd.a' = str_trim(sd.a),
                                                  'sd.b' = str_trim(sd.b),
                                                  'n' = str_trim(n),
                                                  'm.a' = str_trim(m.a),
                                                  'm.b' = str_trim(m.b)),
                                       s))
    }
    
    
    zielwerte.einzeln <- zielwerte.einzeln %>%
      unique() %>%
      filter(zw.a != '-') %>%
      filter(geraet.name != 'Alle Methoden') %>%
      mutate_at(vars(zw.a, zw.b, sd.a, sd.b, n, m.a, m.b), 
                str_replace, pattern = ',', replacement = '.') %>%
      mutate_at(vars(zw.a, zw.b, sd.a, sd.b, n, m.a, m.b), 
                as.numeric) %>%
      mutate(round = r)
  })
  zw.by.year$year <- year 
  zw.by.year
})


zielwerte.rfb.ids2 <- zielwerte.rfb %>%
  mutate(kid = str_match(geraet.name, 'Kit (\\d+)')[,2]) %>%
  #  mutate(kid = ifelse(is.na(kid), 90, kid)) %>%
  mutate(kid = as.numeric(kid)) %>%
  mutate(geraet.name = str_replace(geraet.name, ' - Kit \\d+', '')) %>%
  # unique() %>%
  filter(!is.na(zw.a)) %>%
  mutate(geraet.name.s = str_sub(geraet.name, 1, 30)) %>%
  mutate(kid = as.character(kid)) %>%
  mutate(zid = paste(year, round, kid, geraet.name, sep='-'))

zielwerte.rfb.ids3 <- zielwerte.rfb.ids2 %>%
  select(2:16, 18) %>%
  unique()

eingelesen <- eqaAll %>%
  filter(eqa == 'POCT-RfB') %>%
  group_by(year, round, device) %>%
  summarise(medEingelesen.A = median(value[sample == 1]),
            medEingelesen.B = median(value[sample == 2]),
            nTeiln = n_distinct(pid)) %>%
  ungroup() %>%
  mutate(device = as.character(device),
         uid = 1:n())

eingelesen.Merge <- zielwerte.rfb.ids3 %>%
#  filter(eqa == 'RfB GL') %>%
  left_join(eingelesen, by = c('year' = 'year',
                               'round' = 'round',
                               'geraet.name.s' = 'device')) %>%
  mutate(nDiff = n - nTeiln, 
         medADiff = medEingelesen.A - med.a,
         medBDiff = medEingelesen.B - med.b)

noMatch <- eingelesen.Merge %>% filter(is.na(uid))

noMatchEingelesen <- eingelesen %>%
  filter(!uid %in% eingelesen.Merge$uid)

rtf<-RTF(here::here('tab', 'noMatch.rtf'))
addTable(rtf,noMatch %>% select(1:16))
done(rtf)
