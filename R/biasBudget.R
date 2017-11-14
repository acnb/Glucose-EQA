riskpair <- read_csv(here::here("data", "riskpair.csv"))
lkpRiskCat <- read_csv(here::here("data", "lkpRiskCat.csv"))

segCats <- function(measuredValue, referenceValue){
  breaks <- sort(unique(c(lkpRiskCat$ABSLB, lkpRiskCat$ABSUB)))
  risks <- vector(length = length(measuredValue))
  for(i in 1:length(measuredValue)){
    risks[[i]] <- as.numeric(riskpair[
      riskpair$RefVal == round(referenceValue[i]) &
         riskpair$MeasVal == round(measuredValue[i]),
      "RiskFactor"])
  }

  table(cut(abs(risks), breaks = breaks, labels = lkpRiskCat$RiskCatLabel))
}


biasBudgetSEG.CV <- function(cv, sd=3){
  borders <- data.frame()
  for(i in 0:500){
    temp <- riskpair %>%
      dplyr::filter(RefVal == i) %>%
      summarise(
        ref = i,
        lower = max(MeasVal[RiskFactor >= 1.5]),
        upper = min(MeasVal[RiskFactor <= - 1.5]))
    
    borders <- rbind(borders, temp)
  }
  
  budget <- borders %>%
    mutate(diffUpper = upper - (ref+ref*cv*sd),
           diffLower = lower - (ref-ref*cv*sd)) %>%
    summarise(budgetLower = max(diffLower), 
              refLower = ref[which.max(diffLower)],
              budgetUpper = min(diffUpper), 
              refUpper = ref[which.min(diffUpper)])

  budget
} 


biasBudgetSEG.CharFunc <- function(a, b, sd=3){
  borders <- data.frame()
  for(i in 0:500){
    temp <- riskpair %>%
      dplyr::filter(RefVal == i) %>%
      summarise(
        ref = i,
        lower = max(MeasVal[RiskFactor >= 1.5]),
        upper = min(MeasVal[RiskFactor <= - 1.5]))
    
    borders <- rbind(borders, temp)
  }
  
  budget <- borders %>%
    mutate(diffUpper = upper - (ref+sqrt(a^2+(ref*b)^2)*sd),
           diffLower = lower - (ref-sqrt(a^2+(ref*b)^2)*sd)) %>%
    summarise(budgetLower = max(diffLower), 
              refLower = ref[which.max(diffLower)],
              budgetUpper = min(diffUpper), 
              refUpper = ref[which.min(diffUpper)])
  
  budget
} 


plotAllowedBias <- function(data, title){
  ggplot()+
    geom_rect(data = data, aes(fill = type), 
              xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_errorbar(data = data ,
                  aes(x=charDev, ymin=budgetLower, ymax=budgetUpper, color=eqa),
                  position = position_dodge())+
    scale_y_continuous(sec.axis = 
                         sec_axis(~./mmolConvFactor, 
                                  name = "allowed bias (mmol/L)"), 
                       name='allowed bias (mg/dL)') +
    scale_fill_manual(values = typeColors, guide = "none") + 
    scale_color_manual(values = eqaColors) +
    theme_pub() +
    coord_flip() +
    xlab('') +
    ggtitle(title) +
    facet_grid(type~math, space = 'free_y', scales = 'free_y') +
    theme(legend.title = element_blank(), 
          axis.text.y = element_text(size=8),
          strip.text.y = element_blank())
}


budgetByDevice.CV <- ddply(cv.by.device, c('charDev', 'type', 'eqa'),
                        function(x){
                          biasBudgetSEG.CV(x[1, 'mean.cv.w'])
                        })

rtf<-RTF(here('tab', 'budgetByDevice.CV.rtf'))
addTable(rtf,budgetByDevice.CV)
done(rtf)


budgetByDevice.CharFunc <- ddply(cv.by.device, c('charDev', 'type', 'eqa'),
                           function(x){
                             biasBudgetSEG.CharFunc(x[1, 'a'], x[1, 'b'])
                           })

rtf<-RTF(here('tab', 'budgetByDevice.CharFunc.rtf'))
addTable(rtf,budgetByDevice.CharFunc)
done(rtf)

budgetByDevice.graph <- budgetByDevice.CV %>%
  mutate(math = 'CV') %>%
  rbind(budgetByDevice.CharFunc %>% 
          mutate(math = "characteristic\nfunction")) %>%
  filter(charDev != 'others') %>%
  mutate(charDev = str_replace(charDev, "\n", " ")) %>%
  mutate(charDev = parse_factor(charDev, levels= unique(charDev)))


plotAllowedBias(budgetByDevice.graph, 'bias budget (Surveillance Error Grid)')

ggpub('allowedBiasSEG', height=230)


## bias budget klee simulation ----

biasBudgetSim.CV <- function(cv, sd=3){
  insulinCategoriesUpper <-  
    c(79, 90, 110, 130, 150, 175, 200, 250, 300, 350, 400)
  
  bordersL <- data.frame(ref = insulinCategoriesUpper+1,
                         limit = c(-Inf, -Inf, insulinCategoriesUpper[
                           1:(length(insulinCategoriesUpper)-2)])+1,
                         type = 'lower')
  
  bordersU <- data.frame(ref = insulinCategoriesUpper,
                         limit = c(insulinCategoriesUpper[
                           3:(length(insulinCategoriesUpper))],
                           Inf, Inf),
                         type = 'upper') 
  borders <- rbind(bordersL, bordersU)
  
  budget <- borders %>%
    mutate(diff = abs(ref - limit) - ref*cv*sd) %>%
    group_by(type) %>%
    summarise(budget = min(diff), ref = ref[which.min(diff)]) %>%
    ungroup() %>%
    summarise(budgetLower = -budget[type == 'lower'], 
              refLower = ref[type == 'lower'],
              budgetUpper = budget[type == 'upper'], 
              refUpper = ref[type == 'upper'])
  budget
}


biasBudgetSim.CharFunc <- function(a, b, sd=3){
  insulinCategoriesUpper <-  
    c(79, 90, 110, 130, 150, 175, 200, 250, 300, 350, 400)
  
  bordersL <- data.frame(ref = insulinCategoriesUpper+1,
                         limit = c(-Inf, -Inf, insulinCategoriesUpper[
                           1:(length(insulinCategoriesUpper)-2)])+1,
                         type = 'lower')
  
  bordersU <- data.frame(ref = insulinCategoriesUpper,
                         limit = c(insulinCategoriesUpper[
                          3:(length(insulinCategoriesUpper))],
                          Inf, Inf),
                        type = 'upper') 
  borders <- rbind(bordersL, bordersU)
  
  budget <- borders %>%
    mutate(diff = abs(ref - limit) - (sqrt(a^2+(ref*b)^2)*sd)) %>%
    group_by(type) %>%
    summarise(budget = min(diff), ref = ref[which.min(diff)]) %>%
    ungroup() %>%
    summarise(budgetLower = -budget[type == 'lower'], 
              refLower = ref[type == 'lower'],
              budgetUpper = budget[type == 'upper'], 
              refUpper = ref[type == 'upper'])
  
  budget
} 


budgetByDevice.Sim.CV <- ddply(cv.by.device, c('charDev', 'type', 'eqa'),
                           function(x){
                             biasBudgetSim.CV(x[1, 'mean.cv.w'])
                           })


rtf<-RTF(here('tab', 'budgetByDevice.Sim.CV.rtf'))
addTable(rtf,budgetByDevice.Sim.CV)
done(rtf)


budgetByDevice.Sim.CharFunc <- ddply(cv.by.device, c('charDev', 'type', 'eqa'),
                                 function(x){
                                   biasBudgetSim.CharFunc(x[1, 'a'], x[1, 'b'])
                                 })

rtf<-RTF(here('tab', 'budgetByDevice.Sim.CharFunc.rtf'))
addTable(rtf,budgetByDevice.CharFunc)
done(rtf)


budgetByDevice.Sim.graph <- budgetByDevice.Sim.CV %>%
  mutate(math = 'CV') %>%
  rbind(budgetByDevice.Sim.CharFunc %>% 
          mutate(math = "characteristic\nfunction")) %>%
  filter(charDev != 'others') %>%
  mutate(charDev = str_replace(charDev, "\n", " ")) %>%
  mutate(charDev = parse_factor(charDev, levels= unique(charDev)))



plotAllowedBias(budgetByDevice.Sim.graph, 'bias budget (TGC-Simulation)')

ggpub('allowedBiasSim', height=230)

## summary stats

budgetAll <- rbind(
  budgetByDevice.CV %>%  mutate(math = 'CV', risk = 'SEG'),
  budgetByDevice.CharFunc %>%  mutate(math = 'charFunc', risk = 'SEG'),
  budgetByDevice.Sim.CV %>%  mutate(math = 'CV', risk = 'sim'),
  budgetByDevice.Sim.CharFunc %>% mutate(math = "charFunc", risk = 'sim')
)


statsbudget <- budgetAll %>%
  mutate(budgetLower = budgetLower*-1) %>%
  gather(key = 'direction', value = 'budget', budgetLower, budgetUpper) %>%
  group_by(type) %>%
  summarise(min = min(budget),
            p25 = quantile(budget, names = FALSE, probs=.25),
            med = median(budget),
            p75 = quantile(budget, names = FALSE, probs=.75),
            max = max(budget))

## bias budget explanation graph ----

borders <- data.frame()
for(i in 0:500){
  temp <- riskpair %>%
    dplyr::filter(RefVal == i) %>%
    summarise(
      ref = i,
      lower = max(MeasVal[RiskFactor >= 1.5]),
      upper = min(MeasVal[RiskFactor <= - 1.5]))
  
    borders <- rbind(borders, temp)
}

borders <- as.data.frame(borders) %>%
  mutate(lower = ifelse(is.infinite(lower), NA, lower)) %>%
  mutate(upper = ifelse(is.infinite(upper), NA, upper))


meanCharFunc <- cv.by.device %>%
  filter(charDev != "others") %>%
  filter(type == 'CL') %>%
  summarise(a = median(a), b=median(b)) %>%
  as.list()

charFunc <- function(x){
  sqrt(meanCharFunc[["a"]]^2+(x*meanCharFunc[["b"]])^2)
}

xPos <- borders$ref[which.min(borders$upper-(borders$ref+charFunc(borders$ref)*3))]
xNeg <- borders$ref[which.min((borders$ref-charFunc(borders$ref)*3)-borders$lower)]

budgetPos <- data.frame(
  x = c(xPos, xPos),
  y = c(xPos+3*charFunc(xPos), 
        borders[borders$ref ==xPos, "upper"])
  )

budgetNeg <- data.frame(
  x = c(xNeg, xNeg),
  y = c(xNeg-3*charFunc(xNeg), 
        borders[borders$ref ==xNeg, "lower"])
)

ggplot()+
  geom_point(data = data.frame(x=0, y=0, f=c(0:4)),
             aes(x=x,y=y, fill=f))+
  geom_point(data= riskpair, aes(x=RefVal, y=MeasVal, color = RiskFactor)) +
  geom_line(data = borders, aes(x=ref, y=lower), color='black', 
            size = 1.2, linetype = 'dotted') + 
  geom_line(data = borders, aes(x=ref, y=upper), color='black', 
            size = 1.2, linetype = 'dotted') + 
  scale_fill_gradientn(
    values =scales::rescale(c(0, 1, 2, 3, 3.75)),
    limits=c(0,4),
    colours = c("green", "yellow", "orange", "red", 'brown'),
    guide = guide_colorbar(ticks = FALSE, barheight = unit(100, 'mm')),
    breaks =  c(0.25, 1, 2, 3, 3.75),
    labels = c('none', 'slight', 'moderate', 'high', 'extreme'),
    name = 'risk level'
  ) +
  scale_color_gradientn(
    colours = c("brown", "red", "orange", "yellow", "green",
                "yellow", "orange", "red", 'brown'), 
    guide = 'none',
    limits = c(-4,4), 
    values = scales::rescale(c(-3.75, -3, -2, -1, 0, 1, 2, 3, 3.75))
    ) +
  geom_ribbon(data=data.frame(x=0:300),
              aes(x = x,
                  ymin=pmax(0, x-charFunc(x)*3),
                  ymax=pmin(300, x+charFunc(x)*3)),
              fill = "grey", color='grey', alpha=.8) +
  geom_line(data=budgetPos, aes(x=x, y=y),
            color="blue", size = 1.3) +
  geom_line(data=budgetNeg, aes(x=x, y=y),
            color="blue", size = 1.3) +
  scale_y_continuous(limits = c(0, 300), sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "measured blood glucose (mmol/L)"), 
                     name='measured blood glucose (mg/dL)') +
  scale_x_continuous(limits = c(0, 300), sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "reference blood glucose (mmol/L)"), 
                     name='reference blood glucose (mg/dL)') +
  theme_pub() +
  coord_equal() +
  theme(legend.position = 'right',
        legend.direction = 'vertical')
  
ggpub('biasBudget')
ggpub('final/biasBudget', device = 'tiff', dpi = 600)
