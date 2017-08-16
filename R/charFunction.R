params.single.devices <- eqaAll %>%
  filter(!is.na(sharedDevice)) %>%
  filter(abs(relDiff) < .45) %>%
  group_by(eqa, sharedDevice, rmv) %>%
  filter(n() > 7) %>%
  summarise(sd = getSfromAlgA(value), 
            sdE = getStErrorForS(value),
            targetAlgA = getMufromAlgA(value), n=n()) %>%
  group_by(eqa, sharedDevice) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  ungroup() %>%
  mutate(cv = sd/targetAlgA)


param.char.func <- ddply(params.single.devices, c('eqa', 'sharedDevice'), 
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
  join(param.char.func %>% select(eqa, sharedDevice), type = "inner")


grid <- seq(from=min(params.single.devices$targetAlgA),
            to=max(params.single.devices$targetAlgA), by=.5)

lines.char.func <- ddply(param.char.func, 
                         c('eqa', 'sharedDevice'), 
                         function(x){
                           data.frame(x=grid, 
                                      y= ((x[['a']]^2+(x[['b']]*grid)^2)^.5)/grid)
                           })

ggplot() +
  geom_point(data = params.single.devices, 
             aes(x=targetAlgA, y=cv, alpha=w)) +
  geom_line(data=lines.char.func, aes(x=x, y=y)) +
  facet_wrap(~eqa+sharedDevice) +
  theme_pub(base_size = 10) + 
  theme(legend.position="none", strip.text.x = element_text(size = 6)) +
  ggtitle('characteristic function for device') +
  xlab('assigned value') +
  ylab('coefficient of variation')

ggpub('charFunc', height= 176)


resids <- ddply(params.single.devices, 
                c('eqa', 'sharedDevice'), 
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

ggplot(resids, aes(x=x, y=r, weight=w, alpha=w)) +
  geom_point()+
  geom_smooth()+
  facet_wrap(~eqa+sharedDevice, scales = "free_y") +
  theme_pub(base_size = 10) + 
  theme(legend.position="none") +
  xlab('assigned value') +
  ggtitle('residuals of characteristic function fit') +
  ylab('residuals')

ggpub('residsCharFunc', height= 176)

cv.by.device <- eqaAll %>%
  filter(!is.na(sharedDevice)) %>%
  filter(abs(relDiff) < .45) %>%
  join(param.char.func %>% 
         select(eqa, sharedDevice), type = "inner") %>% # filter
  group_by(eqa, sharedDevice, rmv) %>%
  filter(n() > 7) %>%
  summarise(sd = getSfromAlgA(value), 
            sdE = getStErrorForS(value),
            targetAlgA = getMufromAlgA(value)) %>%
  mutate(cv = sd/targetAlgA) %>%
  group_by(eqa, sharedDevice) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  summarise(mean.cv.w = weighted.mean(cv, w), mean.cv = mean(cv)) %>%
  ungroup()

cv.by.device <- cv.by.device %>%
  join(param.char.func, by=c('eqa' = 'eqa', 
                                   'sharedDevice' = 'sharedDevice')) %>%
  select(sharedDevice, eqa, mean.cv.w, mean.cv, a, b) %>%
  mutate_at(vars(mean.cv.w, mean.cv, a, b), round, digits=3)

cv.by.device.table <- cv.by.device %>%
  mutate(mean = paste0(mean.cv.w, ' (', eqa, ')'),
         alpha = paste0(a, ' (', eqa, ')'),
         beta = paste0(b, ' (', eqa, ')'),
         device = sharedDevice) %>%
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