---
title: "Kauri Dieback Multiple Re-Assessments"
author: "Linley Jesson"
date: '`r Sys.Date()`'
output: html_document
---

```r
knitr::opts_chunk$set(echo = F, message = F)
library(ggplot2)
old <- theme_set(theme_bw()) #capture current theme
#theme_set(old)
library(tidyverse)
library(lubridate)
#suppressMessages(source("Multiple_assessment_data_import_clean.R"))
```

Need to make sure the cross-referenced trees go both ways. So trees in assessment 2 are also in assessment 1 and vice versa.

Note: assessment two likely includes assessment three and perhaps four (by Sept.)


*** Note: Starting health likely a really important parameter. Need to include in each of these.

How many assessments?
```r
Multi_assessments_wide_export %>% 
  select(PIN,Treatment.date, Assessment.date) %>% 
ggplot(., aes(x=Treatment.date, y=Assessment.date, group=PIN))+
  geom_point(aes(colour=PIN))+
  scale_x_date(date_labels = "%b %y")+
  scale_y_date(date_labels = "%b %y")
  
```

For trees that were assessment more than once: How many treatments?
```r
Multi_assessments %>% 
  group_by(Tree_PIN,`Was phosphite applied to this kauri?`,Assessment) %>% 
  summarise(n=n()) %>% 
  group_by(`Was phosphite applied to this kauri?`,Assessment) %>% 
  summarise(`Number of trees`=n())
  
```

Original health - Treated Trees
```{r}
Multi_assessments %>% 
  filter(`Was phosphite applied to this kauri?`=="Yes") %>% 
  filter(Question=="Canopy health score [Score]") %>% 
  group_by(Tree_PIN,Value) %>% 
  summarise(n=n()) %>% 
  group_by(Value) %>% 
  summarise(`Number of trees`=n())

```

Original health - Untreated Trees
```{r}
Multi_assessments %>% 
  filter(`Was phosphite applied to this kauri?`!="Yes") %>% 
  filter(Question=="Canopy health score [Score]") %>% 
  group_by(Tree_PIN,Value) %>% 
  summarise(n=n()) %>% 
  group_by(Value) %>% 
  summarise(`Number of trees`=n())

```

 Do this as histogram, facetted by Phosphite.

Photo comparison summary

# colour this by initial health
```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Canopy photo comparison [Photo comparison]") %>%
  filter(!is.na(Phosphite)) %>% 
  mutate(Value=as.numeric(Value)) %>% {
  ggplot(.,aes(x=TimeSinceTreatment, y=Value))+
  geom_jitter(aes(), height = 0.1)+
      facet_grid(~Phosphite, drop = T)+
  ggtitle(unique(.$Question))
}
```
```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Basal bleed activity") %>% 
  mutate(Value=factor(Value, levels = c("Not active","Semi-active", "Active"))) %>% 
  group_by(`Was phosphite applied to this kauri?`,`What phosphite dose was applied?`,Value) %>% 
  summarise(n=n())
```



```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Basal bleed activity") %>% 
  filter(!is.na(`Was phosphite applied to this kauri?`)) %>% 
  mutate(Value=factor(Value, levels = c("Not active","Semi-active", "Active"))) %>% {
  ggplot(.,aes(y=Value,x=TimeSinceTreatment, group=Tree_PIN))+
  geom_jitter(height = 0.1)+
  facet_grid(`Was phosphite applied to this kauri?`~.)+
  ggtitle(unique(.$Question))
}
```

```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Are there any cracks or bleeds above injection points?") %>% {
  ggplot(.,aes(x=Value))+
  geom_histogram(stat="count") +
      facet_grid(~`What phosphite dose was applied?`)+
  ggtitle(unique(.$Question))
}
```
Does this line up with concentration?
Does this line up with canopy health?
```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Are there any cracks or bleeds above injection points?"|Question=="Canopy health score [Score]") %>% 
  filter(Assessment!="1") %>% 
  spread(key = Question,value = Value)  %>% 
  group_by(Assessment,`Are there any cracks or bleeds above injection points?`,`Canopy health score [Score]`) %>%
  count() %>% 
  group_by(Assessment) %>% 
  mutate(percentage = n/sum(n)) %>% 
  select(-n) %>% 
  mutate(HealthScore=factor(`Canopy health score [Score]`)) %>% 
   {
  ggplot(.,aes(x=HealthScore,y=percentage))+
  geom_point(aes()) +
      facet_grid(`Are there any cracks or bleeds above injection points?`~Assessment)+
  #ggtitle(unique(.$Question))+
      coord_flip()
}
```




Were there more basal bleeds in the second assessment?


```{r}
BasalBleeds_sum <- Multi_assessments %>% 
  ungroup() %>% 
  dplyr::filter(Question=="Are basal bleeds present?") %>% 
  group_by(Assessment,Value, Tree_PIN) %>% 
  summarise(n=n()) %>% 
  group_by(Assessment,Value) %>% 
  summarise(n=n()) 

BasalBleeds <- Multi_assessments %>% 
  ungroup() %>% 
  dplyr::filter(Question=="Are basal bleeds present?") %>% 
  group_by(Assessment,Tree_PIN, Value) %>% 
  summarise(N=n()) %>% 
mutate(AssessValue=paste(Assessment,Value, sep="_")) %>% 
  ungroup() %>% 
  select(-Assessment,-Value) %>% 
   spread(key = AssessValue, value=N) 
BasalBleeds_sum
  
```



```{r}

Multi_assessments %>% 
  dplyr::filter(Question=="Are basal bleeds present?") %>% {
  ggplot(.,aes(x=Value))+
  geom_histogram(stat="count")+
  facet_grid(.~Assessment)+
   ggtitle(unique(.$Question))
    }
```
```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Basal bleed height (cm)") %>% 
  mutate(Value=as.numeric(Value)) %>% {
  ggplot(.,aes(x=Value))+
  geom_histogram()+
  facet_grid(.~Assessment)+
      ggtitle(unique(.$Question))
  }
    
```

```{r}
BasalBleedHeight <- Multi_assessments %>% 
  ungroup() %>% 
  dplyr::filter(Question=="Basal bleed height (cm)") %>% 
   mutate(Tree_PIN_No=paste(Tree_PIN, Number, sep="_")) %>%
  group_by(Assessment,Tree_PIN_No) %>% 
  select(Tree_PIN_No,Assessment,Value) %>% 
  mutate(Number=seq(1:n())) %>%
  mutate(Value=as.numeric(Value)) %>% 
   mutate(Next=as.numeric(lag(Value))) %>% 
  mutate(Diff=Value-Next)

BasalBleedHeight %>% 
  ggplot(., aes(x=Diff))+
  geom_histogram()

```
```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Basal bleed distance") %>% 
  mutate(Value=as.numeric(Value)) %>% {
  ggplot(.,aes(x=Value))+
  geom_histogram()+
  facet_grid(.~Assessment)+
      ggtitle(unique(.$Question))
  }
```

### Moss and lichen score - split by lesion score or regress by lesion score and canopy health score. Predict unhealthy trees have more moss and lichens.

## How does this track if the tree improves health?


```{r}
Multi_assessments %>% 
  dplyr::filter(Question=="Moss and lichen score [Moss score]") %>% 
  mutate(Value=as.numeric(Value)) %>% 
  group_by(`Canopy health score [Score]`,Value,Assessment) %>% 
  summarise(count=n()) %>% 
  filter(Assessment=="1") %>% 
  ggplot(.,aes(x=Value, y=`Canopy health score [Score]`)) +
  geom_point(aes(size=count))+
  facet_grid(~Assessment, labeller = label_both)+
  xlab("Moss and lichen score [Moss score]")
```
## Phosphite application

First treatment:
```{r}
Kauri_DF %>% 
   group_by(`Was phosphite applied to this kauri?`) %>% 
  summarise(n=n())

```

Trees that were followed up for second treatment:
```{r}
Multi_assessments %>% 
  filter(Assessment=="1") %>% 
  group_by(`Was phosphite applied to this kauri?`, Tree_PIN) %>%
  
  summarise(n=n()) %>% 
   group_by(`Was phosphite applied to this kauri?`) %>%
  
  summarise(n=n())


```

```{r}
Multi_assessments %>% 
  filter(Assessment=="1") %>% 
  filter(`Was phosphite applied to this kauri?`=="No") %>% 
 group_by(PIN, Tree_PIN) %>% 
  summarise(n=n()) %>% 
  group_by(PIN) %>% 
  summarise(n=n())

```


```{r}
Multi_assessments %>% 
  filter(Assessment=="1") %>% 
  filter(Question=="Phosphite conc. used") %>% 
  select(`What phosphite dose was applied?`, Question,Value) %>%  
  group_by(`What phosphite dose was applied?`,Value) %>% 
  summarise(n=n())

```

## Reshape this to concentration x dose

```{r}
Multi_assessments %>% 
  filter(Assessment=="1") %>% 
  filter(Question=="What phosphite dose was applied?" |Question== "Phosphite conc. used"  ) %>% 
  spread(Question, Value) %>% 
  group_by(`Phosphite conc. used`, `What phosphite dose was applied?`) %>% 
  summarise(n=n())
```

# If treated how dis that change lesion growth

this graph is clearly not correct. What is up with na/s? Second assessment?
Reshape so assessment is not long.

```{r}
Multi_assessments %>% 
  filter(Assessment=="1") %>% 
  filter(Question=="What phosphite dose was applied?" |Question== "Phosphite conc. used" |Question=="Basal bleed activity" ) %>% 
  spread(Question, Value) %>% 
   ggplot(.,aes(x=`Basal bleed activity`))+
  geom_histogram(stat="count")+
  facet_grid(`What phosphite dose was applied?`~`Phosphite conc. used`)


```

### Were people influenced by disease level on tree when choosing the dose to apply.


