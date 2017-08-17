params.single.devices <- eqaAll %>%
  filter(eqa != 'Instand 100') %>%
  mutate(charDev = ifelse(is.na(sharedDevice) | sharedDevice == 'others', 
                          as.character(device),
                          as.character(sharedDevice))) %>%
  filter(abs(relDiff) < .45) %>%
  filter(n() > 100) %>%
  filter(device != 'others') %>%
  group_by(eqa, charDev, rmv) %>%
  filter(n() > 7) %>%
  summarise(sd = getSfromAlgA(value), 
            sdE = getStErrorForS(value),
            targetAlgA = getMufromAlgA(value), n=n()) %>%
  mutate(cv = sd/targetAlgA) %>%
  group_by(eqa, charDev) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  ungroup() %>%
  mutate(cv = sd/targetAlgA) %>%
  mutate(charDev = ifelse(str_detect(charDev, ' -- '),
    str_sub(charDev, str_locate(charDev, ' -- ')[, 'end']+1),
    charDev)) %>%
  mutate(charDev = str_replace(charDev, '/(\\w{1})', "/ \\1")) %>%
  mutate(type = ifelse(eqa == "RfB KS", 'central lab', 'POCT'))


param.char.func <- ddply(params.single.devices, c('eqa', 'charDev'), 
                             function(x){
  model <- NULL
  try({
    model <- nls(sd ~ (a^2+(b*targetAlgA)^2)^.5, 
                 data=x, weights = x$w,
                 control = nls.control(warnOnly = TRUE), start=c(a=.02, b=.02))
  })
  if(!is.null(model)){
    data.frame(a=coef(model)[['a']],
               b=coef(model)[['b']],
               n = sum(x$n))
  }else{
    data.frame(a=NA,
               b=NA,
               n = sum(x$n))
  }
})

param.char.func <- param.char.func %>%
  filter(n > 100) %>%
  filter(!is.na(a)) 

params.single.devices <- params.single.devices %>%
  join(param.char.func %>% select(eqa, charDev), type = "inner") %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))


grid <- seq(from=min(params.single.devices$targetAlgA),
            to=max(params.single.devices$targetAlgA), by=.5)

lines.char.func <- ddply(param.char.func, 
                         c('eqa', 'charDev'), 
                         function(x){
                           data.frame(x=grid, 
                                      y= ((x[['a']]^2+(x[['b']]*grid)^2)^.5)/grid)
                           })
lines.char.func <- lines.char.func %>%
  mutate(type = ifelse(eqa == "RfB KS", 'central lab', 'POCT')) %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))


eqaColors = c('Instand 800' = '#f5961e',
              'RfB GL' = '#4ba1d1',
              'RfB KS' = '#000000')

typeColors <- c('POCT' = 'white', 'central lab' = '#e9e9e9')

ggplot() +
  geom_rect(data = params.single.devices, aes(fill = type),
              xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point(data = params.single.devices, 
             aes(x=targetAlgA, y=cv, alpha=w, color=eqa)) +
  geom_line(data=lines.char.func, aes(x=x, y=y, color=eqa)) +
  facet_wrap(~charDev, labeller = label_wrap_gen(width=19)) +
  theme_pub(base_size = 10) + 
  scale_alpha(guide = "none") +
  scale_color_manual(values = eqaColors) +
  scale_fill_manual(values = typeColors, guide = "none") + 
  theme(strip.text.x = element_text(size = 5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45)) +
  ggtitle('fitted characteristic function') +
  xlab('assigned value (mg/dl)') +
  ylab('coefficient of variation')

ggpub('charFunc', height = 240)


resids <- ddply(params.single.devices, 
                c('eqa', 'charDev'), 
                function(x){
  model <- NULL
  try({
    model <- nls(sd ~ (a^2+(b*targetAlgA)^2)^.5, data=x, weights = x$w, 
                 control = nls.control(warnOnly = T),  start=c(a=.02, b=.02))
  })
  if(!is.null(model)){
    data.frame(r=resid(model), x=x$targetAlgA, w=x$w)
  }else{
    data.frame(r=NA, x=x$targetAlgA, w=x$w)
  }
})

resids <- resids %>%
  mutate(type = ifelse(eqa == "RfB KS", 'central lab', 'POCT')) %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))

ggplot() +
  geom_rect(data = resids, aes(fill = type), 
            xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point(data = resids, aes(x=x, y=r, alpha=w, color=eqa))+
  geom_smooth(data = resids, aes(x=x, y=r, weight=w, color=eqa))+
  facet_wrap(~charDev, labeller = label_wrap_gen(width=19)) +
  theme_pub(base_size = 10) + 
  scale_alpha(guide = "none") +
  scale_fill_manual(values = typeColors, guide = "none") + 
  scale_color_manual(values = eqaColors) +
  theme(strip.text.x = element_text(size = 5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45)) +
  xlab('assigned value (mg/dl)') +
  ggtitle('residuals of characteristic function fit') +
  ylab('residuals (mg/dl)')

ggpub('residsCharFunc', height = 240)

## table ----

cv.by.device <-  params.single.devices %>%
  group_by(eqa, charDev) %>%
  summarise(mean.cv.w = weighted.mean(cv, w), mean.cv = mean(cv)) %>%
  ungroup() %>%
  join(param.char.func, by=c('eqa' = 'eqa', 
                                   'charDev' = 'charDev')) %>%
  select(charDev, eqa, mean.cv.w, mean.cv, a, b) %>%
  mutate_at(vars(mean.cv.w, mean.cv, a, b), round, digits=3)

cv.by.device.table <- cv.by.device %>%
  mutate(mean = paste0(mean.cv.w, ' (', eqa, ')'),
         alpha = paste0(a, ' (', eqa, ')'),
         beta = paste0(b, ' (', eqa, ')'),
         device = charDev) %>%
  group_by(device) %>%
  summarise(
    type = ifelse(eqa[1] == 'RfB KS', 'central lab', 'POCT'),
    mean = paste0(mean, collapse = ", \n"),
    alpha = paste0(alpha, collapse = ", \n"),
    beta = paste0(beta, collapse = ", \n")) %>%
  ungroup() %>%
  filter(device != 'others') %>%
  arrange(type, device)

rtf<-RTF(here('tab', 'precision.rtf'))
addTable(rtf,cv.by.device.table)
done(rtf)
