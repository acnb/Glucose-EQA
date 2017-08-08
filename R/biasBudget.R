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
    summarise(budgetLowerAbs = max(diffLower), 
              refLowerAbs = ref[which.max(diffLower)],
              budgetUpperAbs = min(diffUpper), 
              refUpperAbs = ref[which.min(diffUpper)],
              budgetLowerRel = max(diffLower/(0:500)), 
              refLowerRel = ref[which.max(diffLower/(0:500))],
              budgetUpperRel = min(diffUpper/(0:500)), 
              refUpperRel = ref[which.min(diffUpper/(0:500))])

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


budgetByDevice.CV <- ddply(cv.by.device, c('sharedDevice', 'eqa'),
                        function(x){
                          biasBudgetSEG.CV(x[1, 'mean.cv.w'])
                        })

rtf<-RTF(here('tab', 'budgetByDevice.CV.csv'))
addTable(rtf,budgetByDevice.CV)
done(rtf)


budgetByDevice.CharFunc <- ddply(cv.by.device, c('sharedDevice', 'eqa'),
                           function(x){
                             biasBudgetSEG.CharFunc(x[1, 'a'], x[1, 'b'])
                           })

rtf<-RTF(here('tab', 'budgetByDevice.CharFunc.csv'))
addTable(rtf,budgetByDevice.CharFunc)
done(rtf)


ggplot(budgetByDevice.CV %>% filter(sharedDevice != 'others'),
       aes(x=sharedDevice, ymin=budgetLower, 
                              ymax=budgetUpper, color=eqa))+
  geom_errorbar()+
  theme_pub() +
  ylab('allowed bias (mg/dl)') +
  xlab('device') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())

ggpub('allowedBias')

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
  filter(sharedDevice != "others") %>%
  filter(eqa == 'Instand 800' | eqa == "RfB GL") %>%
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
  geom_point(data= riskpair, aes(x=RefVal, y=MeasVal, color = RiskFactor*2)) +
  geom_line(data = borders, aes(x=ref, y=lower), color='black') + 
  geom_line(data = borders, aes(x=ref, y=upper), color='black') + 
  scale_color_gradientn(
    colours = c("brown", "red", "orange", "yellow", "green",
                "yellow", "orange", "red", 'brown'), 
    values = scales::rescale(c(-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5))
    ) +
  geom_ribbon(data=data.frame(x=0:300),
              aes(x = x, 
                  ymin=pmax(0, x-charFunc(x)*3), 
                  ymax=pmin(300, x+charFunc(x)*3)), 
              fill = "grey50", color='grey50', alpha=.5) +
  geom_line(data=budgetPos, aes(x=x, y=y), 
            color="blue", size = 1.2) + 
  geom_line(data=budgetNeg, aes(x=x, y=y), 
            color="blue", size = 1.2) + 
  scale_x_continuous(limits = c(0, 300)) +
  scale_y_continuous(limits = c(0, 300)) +
  xlab('true value (mg/dl)') + 
  ylab('measured value (mg/dl)')+
  theme_pub() +
  theme(legend.position = "none")
  
ggpub('biasBudget')