---
title: "Hedonic S2 template"
output: 
  html_document:
    keep_md: true
---




First read in the packages, data and split the selections variable into cultivar, site, and graft year. Note that the selection coding can have errors or not be consistant with the delimeters and ordering. An example of Green S2 2019 is used.


```r
library(readxl)
library(lmerTest)
library(lsmeans)
library(kableExtra)
library(tidyr)
library(dplyr)
library(emmeans)

Green_hedonic <- read_excel("479_Block3 Aggregated Green_cleaned.xlsx", 
     sheet = "Q1 Hedonic")[,1:11]

Green_hedonic<-Green_hedonic[Green_hedonic$selections!="G102-15.WGT",]


Green_hedonic[Green_hedonic$selections=="G102-15-run2.WGT",]$selections<-"G102.15.WGT"
Green_hedonic[Green_hedonic$selections=="G177-16.TRC",]$selections<-"G117-16.TRC"





Green_hedonic$selections<-gsub("-", ".", Green_hedonic$selections)
#Green_cata$selections<-gsub("-", ".", Green_cata$selections)


tmp<-data.frame(do.call('rbind',(strsplit(as.character(Green_hedonic$selections),'.',fixed=TRUE))))

Green_hedonic$cultivar<-factor(tmp$X1)
Green_hedonic$site<-factor(tmp$X3)
Green_hedonic$graft_year<-factor(tmp$X2)

Green_hedonic$int_appear<-as.numeric(Green_hedonic$`1b) Internal appearance H`)
Green_hedonic$liking<-as.numeric(Green_hedonic$`1a) Overall H`)
Green_hedonic<-data.frame(Green_hedonic)
```

Predict hedonic by cultivar for liking. 
First fit linear mixed models and get comparison with the control. The CLD letters will be deprecated in the future. Make sure to change the reference in the contrast function to the correct cultivar. You may also want to include graft year, however the coding has been inconsistant in previous years with some selections having multiple graft years.
  
  

```
## Warning: 'CLD' will be deprecated. Its use is discouraged.
## See '? CLD' for an explanation. Use 'pwpp' or 'multcomp::cld' instead.
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> cultivar </th>
   <th style="text-align:right;"> emmean </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> lower.CL </th>
   <th style="text-align:right;"> upper.CL </th>
   <th style="text-align:left;"> .group </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> G118 </td>
   <td style="text-align:right;"> 5.100895 </td>
   <td style="text-align:right;"> 0.3092488 </td>
   <td style="text-align:right;"> 19.02761 </td>
   <td style="text-align:right;"> 4.453694 </td>
   <td style="text-align:right;"> 5.748097 </td>
   <td style="text-align:left;"> A </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> G133 </td>
   <td style="text-align:right;"> 5.569904 </td>
   <td style="text-align:right;"> 0.3689734 </td>
   <td style="text-align:right;"> 23.63862 </td>
   <td style="text-align:right;"> 4.807764 </td>
   <td style="text-align:right;"> 6.332045 </td>
   <td style="text-align:left;"> AB </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> G121 </td>
   <td style="text-align:right;"> 5.702405 </td>
   <td style="text-align:right;"> 0.3719963 </td>
   <td style="text-align:right;"> 23.68545 </td>
   <td style="text-align:right;"> 4.934102 </td>
   <td style="text-align:right;"> 6.470707 </td>
   <td style="text-align:left;"> ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> G143 </td>
   <td style="text-align:right;"> 5.804363 </td>
   <td style="text-align:right;"> 0.3710244 </td>
   <td style="text-align:right;"> 24.17876 </td>
   <td style="text-align:right;"> 5.038906 </td>
   <td style="text-align:right;"> 6.569820 </td>
   <td style="text-align:left;"> ABCD </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> G131 </td>
   <td style="text-align:right;"> 5.916656 </td>
   <td style="text-align:right;"> 0.3201927 </td>
   <td style="text-align:right;"> 18.20638 </td>
   <td style="text-align:right;"> 5.244503 </td>
   <td style="text-align:right;"> 6.588810 </td>
   <td style="text-align:left;"> BCD </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> G149 </td>
   <td style="text-align:right;"> 5.944904 </td>
   <td style="text-align:right;"> 0.3689734 </td>
   <td style="text-align:right;"> 23.63862 </td>
   <td style="text-align:right;"> 5.182764 </td>
   <td style="text-align:right;"> 6.707045 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> G113 </td>
   <td style="text-align:right;"> 5.973155 </td>
   <td style="text-align:right;"> 0.3738921 </td>
   <td style="text-align:right;"> 24.18655 </td>
   <td style="text-align:right;"> 5.201794 </td>
   <td style="text-align:right;"> 6.744516 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> Hay </td>
   <td style="text-align:right;"> 5.993543 </td>
   <td style="text-align:right;"> 0.3203478 </td>
   <td style="text-align:right;"> 18.23550 </td>
   <td style="text-align:right;"> 5.321139 </td>
   <td style="text-align:right;"> 6.665946 </td>
   <td style="text-align:left;"> BCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> G126 </td>
   <td style="text-align:right;"> 5.998347 </td>
   <td style="text-align:right;"> 0.3201602 </td>
   <td style="text-align:right;"> 18.19936 </td>
   <td style="text-align:right;"> 5.326243 </td>
   <td style="text-align:right;"> 6.670451 </td>
   <td style="text-align:left;"> BCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> G122 </td>
   <td style="text-align:right;"> 6.056685 </td>
   <td style="text-align:right;"> 0.3207215 </td>
   <td style="text-align:right;"> 18.33201 </td>
   <td style="text-align:right;"> 5.383747 </td>
   <td style="text-align:right;"> 6.729622 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> G112 </td>
   <td style="text-align:right;"> 6.057321 </td>
   <td style="text-align:right;"> 0.3709879 </td>
   <td style="text-align:right;"> 24.17007 </td>
   <td style="text-align:right;"> 5.291925 </td>
   <td style="text-align:right;"> 6.822718 </td>
   <td style="text-align:left;"> BCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> G099 </td>
   <td style="text-align:right;"> 6.094905 </td>
   <td style="text-align:right;"> 0.3689734 </td>
   <td style="text-align:right;"> 23.63862 </td>
   <td style="text-align:right;"> 5.332764 </td>
   <td style="text-align:right;"> 6.857045 </td>
   <td style="text-align:left;"> BCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> G137 </td>
   <td style="text-align:right;"> 6.153561 </td>
   <td style="text-align:right;"> 0.3720107 </td>
   <td style="text-align:right;"> 23.69122 </td>
   <td style="text-align:right;"> 5.385238 </td>
   <td style="text-align:right;"> 6.921883 </td>
   <td style="text-align:left;"> BCDEFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> G106 </td>
   <td style="text-align:right;"> 6.245336 </td>
   <td style="text-align:right;"> 0.3209139 </td>
   <td style="text-align:right;"> 18.36805 </td>
   <td style="text-align:right;"> 5.572088 </td>
   <td style="text-align:right;"> 6.918584 </td>
   <td style="text-align:left;"> CDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> G109 </td>
   <td style="text-align:right;"> 6.263997 </td>
   <td style="text-align:right;"> 0.3214488 </td>
   <td style="text-align:right;"> 18.50333 </td>
   <td style="text-align:right;"> 5.589973 </td>
   <td style="text-align:right;"> 6.938021 </td>
   <td style="text-align:left;"> CDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> G139 </td>
   <td style="text-align:right;"> 6.269904 </td>
   <td style="text-align:right;"> 0.3689734 </td>
   <td style="text-align:right;"> 23.63862 </td>
   <td style="text-align:right;"> 5.507764 </td>
   <td style="text-align:right;"> 7.032045 </td>
   <td style="text-align:left;"> BCDEFGHIJK </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> G011 </td>
   <td style="text-align:right;"> 6.281551 </td>
   <td style="text-align:right;"> 0.3397702 </td>
   <td style="text-align:right;"> 16.23875 </td>
   <td style="text-align:right;"> 5.562130 </td>
   <td style="text-align:right;"> 7.000972 </td>
   <td style="text-align:left;"> BCDEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> G115 </td>
   <td style="text-align:right;"> 6.310110 </td>
   <td style="text-align:right;"> 0.3214718 </td>
   <td style="text-align:right;"> 18.50250 </td>
   <td style="text-align:right;"> 5.636035 </td>
   <td style="text-align:right;"> 6.984184 </td>
   <td style="text-align:left;"> CDEFGHIJK </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> G125 </td>
   <td style="text-align:right;"> 6.311199 </td>
   <td style="text-align:right;"> 0.3214922 </td>
   <td style="text-align:right;"> 18.51429 </td>
   <td style="text-align:right;"> 5.637111 </td>
   <td style="text-align:right;"> 6.985287 </td>
   <td style="text-align:left;"> CDEFGHIJK </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> G129 </td>
   <td style="text-align:right;"> 6.317172 </td>
   <td style="text-align:right;"> 0.3720912 </td>
   <td style="text-align:right;"> 23.70740 </td>
   <td style="text-align:right;"> 5.548711 </td>
   <td style="text-align:right;"> 7.085632 </td>
   <td style="text-align:left;"> BCDEFGHIJK </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> G117 </td>
   <td style="text-align:right;"> 6.351365 </td>
   <td style="text-align:right;"> 0.3207795 </td>
   <td style="text-align:right;"> 18.34103 </td>
   <td style="text-align:right;"> 5.678329 </td>
   <td style="text-align:right;"> 7.024400 </td>
   <td style="text-align:left;"> CDEFGHIJK </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> G100 </td>
   <td style="text-align:right;"> 6.363918 </td>
   <td style="text-align:right;"> 0.3238544 </td>
   <td style="text-align:right;"> 17.43066 </td>
   <td style="text-align:right;"> 5.681928 </td>
   <td style="text-align:right;"> 7.045907 </td>
   <td style="text-align:left;"> CDEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> G128 </td>
   <td style="text-align:right;"> 6.367020 </td>
   <td style="text-align:right;"> 0.3740256 </td>
   <td style="text-align:right;"> 24.22428 </td>
   <td style="text-align:right;"> 5.595447 </td>
   <td style="text-align:right;"> 7.138593 </td>
   <td style="text-align:left;"> CDEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> G136 </td>
   <td style="text-align:right;"> 6.487606 </td>
   <td style="text-align:right;"> 0.3888713 </td>
   <td style="text-align:right;"> 23.75062 </td>
   <td style="text-align:right;"> 5.684569 </td>
   <td style="text-align:right;"> 7.290643 </td>
   <td style="text-align:left;"> BCDEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> G123 </td>
   <td style="text-align:right;"> 6.492172 </td>
   <td style="text-align:right;"> 0.3720912 </td>
   <td style="text-align:right;"> 23.70740 </td>
   <td style="text-align:right;"> 5.723711 </td>
   <td style="text-align:right;"> 7.260632 </td>
   <td style="text-align:left;"> DEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> G140 </td>
   <td style="text-align:right;"> 6.503718 </td>
   <td style="text-align:right;"> 0.3235178 </td>
   <td style="text-align:right;"> 17.29284 </td>
   <td style="text-align:right;"> 5.822035 </td>
   <td style="text-align:right;"> 7.185402 </td>
   <td style="text-align:left;"> DEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> G119 </td>
   <td style="text-align:right;"> 6.535177 </td>
   <td style="text-align:right;"> 0.3208459 </td>
   <td style="text-align:right;"> 18.35188 </td>
   <td style="text-align:right;"> 5.862031 </td>
   <td style="text-align:right;"> 7.208324 </td>
   <td style="text-align:left;"> EFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> G107 </td>
   <td style="text-align:right;"> 6.535539 </td>
   <td style="text-align:right;"> 0.3209139 </td>
   <td style="text-align:right;"> 18.36805 </td>
   <td style="text-align:right;"> 5.862291 </td>
   <td style="text-align:right;"> 7.208787 </td>
   <td style="text-align:left;"> FGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> G095 </td>
   <td style="text-align:right;"> 6.543410 </td>
   <td style="text-align:right;"> 0.3114458 </td>
   <td style="text-align:right;"> 19.20590 </td>
   <td style="text-align:right;"> 5.892019 </td>
   <td style="text-align:right;"> 7.194800 </td>
   <td style="text-align:left;"> EFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> V002 </td>
   <td style="text-align:right;"> 6.669129 </td>
   <td style="text-align:right;"> 0.3207679 </td>
   <td style="text-align:right;"> 18.33478 </td>
   <td style="text-align:right;"> 5.996102 </td>
   <td style="text-align:right;"> 7.342156 </td>
   <td style="text-align:left;"> HIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> G142 </td>
   <td style="text-align:right;"> 6.674712 </td>
   <td style="text-align:right;"> 0.3740256 </td>
   <td style="text-align:right;"> 24.22428 </td>
   <td style="text-align:right;"> 5.903139 </td>
   <td style="text-align:right;"> 7.446285 </td>
   <td style="text-align:left;"> GHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> G108 </td>
   <td style="text-align:right;"> 6.677779 </td>
   <td style="text-align:right;"> 0.3739178 </td>
   <td style="text-align:right;"> 24.20273 </td>
   <td style="text-align:right;"> 5.906392 </td>
   <td style="text-align:right;"> 7.449166 </td>
   <td style="text-align:left;"> GHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> G135 </td>
   <td style="text-align:right;"> 6.685022 </td>
   <td style="text-align:right;"> 0.3709358 </td>
   <td style="text-align:right;"> 24.16777 </td>
   <td style="text-align:right;"> 5.919729 </td>
   <td style="text-align:right;"> 7.450315 </td>
   <td style="text-align:left;"> GHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> G102 </td>
   <td style="text-align:right;"> 6.688063 </td>
   <td style="text-align:right;"> 0.3213436 </td>
   <td style="text-align:right;"> 18.47886 </td>
   <td style="text-align:right;"> 6.014197 </td>
   <td style="text-align:right;"> 7.361929 </td>
   <td style="text-align:left;"> HIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> G130 </td>
   <td style="text-align:right;"> 6.785962 </td>
   <td style="text-align:right;"> 0.3201867 </td>
   <td style="text-align:right;"> 18.20298 </td>
   <td style="text-align:right;"> 6.113812 </td>
   <td style="text-align:right;"> 7.458112 </td>
   <td style="text-align:left;"> IJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> G127 </td>
   <td style="text-align:right;"> 6.816813 </td>
   <td style="text-align:right;"> 0.3214100 </td>
   <td style="text-align:right;"> 18.49794 </td>
   <td style="text-align:right;"> 6.142856 </td>
   <td style="text-align:right;"> 7.490769 </td>
   <td style="text-align:left;"> JKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> G120 </td>
   <td style="text-align:right;"> 6.817295 </td>
   <td style="text-align:right;"> 0.3889035 </td>
   <td style="text-align:right;"> 23.75490 </td>
   <td style="text-align:right;"> 6.014200 </td>
   <td style="text-align:right;"> 7.620391 </td>
   <td style="text-align:left;"> EFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> G124 </td>
   <td style="text-align:right;"> 6.966025 </td>
   <td style="text-align:right;"> 0.3721186 </td>
   <td style="text-align:right;"> 23.70894 </td>
   <td style="text-align:right;"> 6.197510 </td>
   <td style="text-align:right;"> 7.734539 </td>
   <td style="text-align:left;"> KL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> G145 </td>
   <td style="text-align:right;"> 7.100311 </td>
   <td style="text-align:right;"> 0.3740277 </td>
   <td style="text-align:right;"> 24.21843 </td>
   <td style="text-align:right;"> 6.328724 </td>
   <td style="text-align:right;"> 7.871898 </td>
   <td style="text-align:left;"> L </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> contrast </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> t.ratio </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> G011 - Hay </td>
   <td style="text-align:right;"> 0.2880081 </td>
   <td style="text-align:right;"> 0.3766565 </td>
   <td style="text-align:right;"> 19.50611 </td>
   <td style="text-align:right;"> 0.7646440 </td>
   <td style="text-align:right;"> 0.4536354 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G095 - Hay </td>
   <td style="text-align:right;"> 0.5498671 </td>
   <td style="text-align:right;"> 0.2857671 </td>
   <td style="text-align:right;"> 17.72102 </td>
   <td style="text-align:right;"> 1.9241792 </td>
   <td style="text-align:right;"> 0.0705437 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G099 - Hay </td>
   <td style="text-align:right;"> 0.1013620 </td>
   <td style="text-align:right;"> 0.3158767 </td>
   <td style="text-align:right;"> 15.87558 </td>
   <td style="text-align:right;"> 0.3208912 </td>
   <td style="text-align:right;"> 0.7524784 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G100 - Hay </td>
   <td style="text-align:right;"> 0.3703751 </td>
   <td style="text-align:right;"> 0.2909650 </td>
   <td style="text-align:right;"> 17.81867 </td>
   <td style="text-align:right;"> 1.2729199 </td>
   <td style="text-align:right;"> 0.2194047 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G102 - Hay </td>
   <td style="text-align:right;"> 0.6945204 </td>
   <td style="text-align:right;"> 0.2564104 </td>
   <td style="text-align:right;"> 16.17353 </td>
   <td style="text-align:right;"> 2.7086286 </td>
   <td style="text-align:right;"> 0.0153871 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G106 - Hay </td>
   <td style="text-align:right;"> 0.2517932 </td>
   <td style="text-align:right;"> 0.2556158 </td>
   <td style="text-align:right;"> 15.97186 </td>
   <td style="text-align:right;"> 0.9850455 </td>
   <td style="text-align:right;"> 0.3392896 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G107 - Hay </td>
   <td style="text-align:right;"> 0.5419970 </td>
   <td style="text-align:right;"> 0.2556158 </td>
   <td style="text-align:right;"> 15.97186 </td>
   <td style="text-align:right;"> 2.1203577 </td>
   <td style="text-align:right;"> 0.0499857 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G108 - Hay </td>
   <td style="text-align:right;"> 0.6842366 </td>
   <td style="text-align:right;"> 0.3180425 </td>
   <td style="text-align:right;"> 16.31644 </td>
   <td style="text-align:right;"> 2.1513998 </td>
   <td style="text-align:right;"> 0.0467513 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G109 - Hay </td>
   <td style="text-align:right;"> 0.2704545 </td>
   <td style="text-align:right;"> 0.2563503 </td>
   <td style="text-align:right;"> 16.15860 </td>
   <td style="text-align:right;"> 1.0550194 </td>
   <td style="text-align:right;"> 0.3069366 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G112 - Hay </td>
   <td style="text-align:right;"> 0.0637787 </td>
   <td style="text-align:right;"> 0.3182211 </td>
   <td style="text-align:right;"> 16.35235 </td>
   <td style="text-align:right;"> 0.2004225 </td>
   <td style="text-align:right;"> 0.8436217 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G113 - Hay </td>
   <td style="text-align:right;"> -0.0203876 </td>
   <td style="text-align:right;"> 0.3181259 </td>
   <td style="text-align:right;"> 16.33321 </td>
   <td style="text-align:right;"> -0.0640865 </td>
   <td style="text-align:right;"> 0.9496792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G115 - Hay </td>
   <td style="text-align:right;"> 0.3165673 </td>
   <td style="text-align:right;"> 0.2566414 </td>
   <td style="text-align:right;"> 16.23105 </td>
   <td style="text-align:right;"> 1.2335006 </td>
   <td style="text-align:right;"> 0.2349541 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G117 - Hay </td>
   <td style="text-align:right;"> 0.3578224 </td>
   <td style="text-align:right;"> 0.2557999 </td>
   <td style="text-align:right;"> 16.01741 </td>
   <td style="text-align:right;"> 1.3988376 </td>
   <td style="text-align:right;"> 0.1809267 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G118 - Hay </td>
   <td style="text-align:right;"> -0.8926471 </td>
   <td style="text-align:right;"> 0.2848622 </td>
   <td style="text-align:right;"> 17.38742 </td>
   <td style="text-align:right;"> -3.1336101 </td>
   <td style="text-align:right;"> 0.0059264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G119 - Hay </td>
   <td style="text-align:right;"> 0.5416350 </td>
   <td style="text-align:right;"> 0.2556760 </td>
   <td style="text-align:right;"> 15.98642 </td>
   <td style="text-align:right;"> 2.1184428 </td>
   <td style="text-align:right;"> 0.0501543 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G120 - Hay </td>
   <td style="text-align:right;"> 0.8237530 </td>
   <td style="text-align:right;"> 0.3994177 </td>
   <td style="text-align:right;"> 18.87924 </td>
   <td style="text-align:right;"> 2.0623845 </td>
   <td style="text-align:right;"> 0.0532057 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G121 - Hay </td>
   <td style="text-align:right;"> -0.2911375 </td>
   <td style="text-align:right;"> 0.3159019 </td>
   <td style="text-align:right;"> 15.88065 </td>
   <td style="text-align:right;"> -0.9216075 </td>
   <td style="text-align:right;"> 0.3705231 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G122 - Hay </td>
   <td style="text-align:right;"> 0.0631422 </td>
   <td style="text-align:right;"> 0.2557251 </td>
   <td style="text-align:right;"> 15.99846 </td>
   <td style="text-align:right;"> 0.2469143 </td>
   <td style="text-align:right;"> 0.8081150 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G123 - Hay </td>
   <td style="text-align:right;"> 0.4986291 </td>
   <td style="text-align:right;"> 0.3157710 </td>
   <td style="text-align:right;"> 15.85461 </td>
   <td style="text-align:right;"> 1.5790846 </td>
   <td style="text-align:right;"> 0.1340582 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G124 - Hay </td>
   <td style="text-align:right;"> 0.9724822 </td>
   <td style="text-align:right;"> 0.3157131 </td>
   <td style="text-align:right;"> 15.84300 </td>
   <td style="text-align:right;"> 3.0802720 </td>
   <td style="text-align:right;"> 0.0072358 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G125 - Hay </td>
   <td style="text-align:right;"> 0.3176563 </td>
   <td style="text-align:right;"> 0.2564366 </td>
   <td style="text-align:right;"> 16.18019 </td>
   <td style="text-align:right;"> 1.2387327 </td>
   <td style="text-align:right;"> 0.2331150 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G126 - Hay </td>
   <td style="text-align:right;"> 0.0048043 </td>
   <td style="text-align:right;"> 0.2552800 </td>
   <td style="text-align:right;"> 15.88873 </td>
   <td style="text-align:right;"> 0.0188198 </td>
   <td style="text-align:right;"> 0.9852192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G127 - Hay </td>
   <td style="text-align:right;"> 0.8232701 </td>
   <td style="text-align:right;"> 0.2563783 </td>
   <td style="text-align:right;"> 16.16561 </td>
   <td style="text-align:right;"> 3.2111529 </td>
   <td style="text-align:right;"> 0.0053911 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G128 - Hay </td>
   <td style="text-align:right;"> 0.3734772 </td>
   <td style="text-align:right;"> 0.3180457 </td>
   <td style="text-align:right;"> 16.31694 </td>
   <td style="text-align:right;"> 1.1742881 </td>
   <td style="text-align:right;"> 0.2571341 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G129 - Hay </td>
   <td style="text-align:right;"> 0.3236291 </td>
   <td style="text-align:right;"> 0.3157710 </td>
   <td style="text-align:right;"> 15.85461 </td>
   <td style="text-align:right;"> 1.0248855 </td>
   <td style="text-align:right;"> 0.3208012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G130 - Hay </td>
   <td style="text-align:right;"> 0.7924192 </td>
   <td style="text-align:right;"> 0.2550244 </td>
   <td style="text-align:right;"> 15.82650 </td>
   <td style="text-align:right;"> 3.1072287 </td>
   <td style="text-align:right;"> 0.0068469 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G131 - Hay </td>
   <td style="text-align:right;"> -0.0768862 </td>
   <td style="text-align:right;"> 0.2550299 </td>
   <td style="text-align:right;"> 15.82775 </td>
   <td style="text-align:right;"> -0.3014793 </td>
   <td style="text-align:right;"> 0.7669739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G133 - Hay </td>
   <td style="text-align:right;"> -0.4236380 </td>
   <td style="text-align:right;"> 0.3158767 </td>
   <td style="text-align:right;"> 15.87558 </td>
   <td style="text-align:right;"> -1.3411500 </td>
   <td style="text-align:right;"> 0.1987452 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G135 - Hay </td>
   <td style="text-align:right;"> 0.6914797 </td>
   <td style="text-align:right;"> 0.3180469 </td>
   <td style="text-align:right;"> 16.31725 </td>
   <td style="text-align:right;"> 2.1741443 </td>
   <td style="text-align:right;"> 0.0447350 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G136 - Hay </td>
   <td style="text-align:right;"> 0.4940634 </td>
   <td style="text-align:right;"> 0.3993842 </td>
   <td style="text-align:right;"> 18.87314 </td>
   <td style="text-align:right;"> 1.2370631 </td>
   <td style="text-align:right;"> 0.2312357 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G137 - Hay </td>
   <td style="text-align:right;"> 0.1600182 </td>
   <td style="text-align:right;"> 0.3156728 </td>
   <td style="text-align:right;"> 15.83512 </td>
   <td style="text-align:right;"> 0.5069116 </td>
   <td style="text-align:right;"> 0.6192026 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G139 - Hay </td>
   <td style="text-align:right;"> 0.2763620 </td>
   <td style="text-align:right;"> 0.3158767 </td>
   <td style="text-align:right;"> 15.87558 </td>
   <td style="text-align:right;"> 0.8749049 </td>
   <td style="text-align:right;"> 0.3946696 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G140 - Hay </td>
   <td style="text-align:right;"> 0.5101761 </td>
   <td style="text-align:right;"> 0.2908615 </td>
   <td style="text-align:right;"> 17.78524 </td>
   <td style="text-align:right;"> 1.7540171 </td>
   <td style="text-align:right;"> 0.0966423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G142 - Hay </td>
   <td style="text-align:right;"> 0.6811695 </td>
   <td style="text-align:right;"> 0.3180457 </td>
   <td style="text-align:right;"> 16.31694 </td>
   <td style="text-align:right;"> 2.1417351 </td>
   <td style="text-align:right;"> 0.0476322 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G143 - Hay </td>
   <td style="text-align:right;"> -0.1891792 </td>
   <td style="text-align:right;"> 0.3179402 </td>
   <td style="text-align:right;"> 16.29560 </td>
   <td style="text-align:right;"> -0.5950150 </td>
   <td style="text-align:right;"> 0.5600061 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G145 - Hay </td>
   <td style="text-align:right;"> 1.1067686 </td>
   <td style="text-align:right;"> 0.3180339 </td>
   <td style="text-align:right;"> 16.31450 </td>
   <td style="text-align:right;"> 3.4800338 </td>
   <td style="text-align:right;"> 0.0030162 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G149 - Hay </td>
   <td style="text-align:right;"> -0.0486380 </td>
   <td style="text-align:right;"> 0.3158767 </td>
   <td style="text-align:right;"> 15.87558 </td>
   <td style="text-align:right;"> -0.1539777 </td>
   <td style="text-align:right;"> 0.8795677 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V002 - Hay </td>
   <td style="text-align:right;"> 0.6755865 </td>
   <td style="text-align:right;"> 0.2557252 </td>
   <td style="text-align:right;"> 15.99930 </td>
   <td style="text-align:right;"> 2.6418455 </td>
   <td style="text-align:right;"> 0.0177626 </td>
  </tr>
</tbody>
</table>

Repeat for internal appearance or other hedonic measures


```
## Warning: 'CLD' will be deprecated. Its use is discouraged.
## See '? CLD' for an explanation. Use 'pwpp' or 'multcomp::cld' instead.
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> cultivar </th>
   <th style="text-align:right;"> emmean </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> lower.CL </th>
   <th style="text-align:right;"> upper.CL </th>
   <th style="text-align:left;"> .group </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> G131 </td>
   <td style="text-align:right;"> 6.293775 </td>
   <td style="text-align:right;"> 0.2181296 </td>
   <td style="text-align:right;"> 56.54309 </td>
   <td style="text-align:right;"> 5.856901 </td>
   <td style="text-align:right;"> 6.730648 </td>
   <td style="text-align:left;"> A </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> G143 </td>
   <td style="text-align:right;"> 6.441793 </td>
   <td style="text-align:right;"> 0.2588567 </td>
   <td style="text-align:right;"> 48.24005 </td>
   <td style="text-align:right;"> 5.921394 </td>
   <td style="text-align:right;"> 6.962192 </td>
   <td style="text-align:left;"> ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> G118 </td>
   <td style="text-align:right;"> 6.458163 </td>
   <td style="text-align:right;"> 0.2172559 </td>
   <td style="text-align:right;"> 58.69033 </td>
   <td style="text-align:right;"> 6.023387 </td>
   <td style="text-align:right;"> 6.892939 </td>
   <td style="text-align:left;"> AB </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> G121 </td>
   <td style="text-align:right;"> 6.523118 </td>
   <td style="text-align:right;"> 0.2583086 </td>
   <td style="text-align:right;"> 47.07923 </td>
   <td style="text-align:right;"> 6.003491 </td>
   <td style="text-align:right;"> 7.042745 </td>
   <td style="text-align:left;"> ABCD </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> G123 </td>
   <td style="text-align:right;"> 6.533296 </td>
   <td style="text-align:right;"> 0.2583801 </td>
   <td style="text-align:right;"> 47.12376 </td>
   <td style="text-align:right;"> 6.013538 </td>
   <td style="text-align:right;"> 7.053053 </td>
   <td style="text-align:left;"> ABCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> G149 </td>
   <td style="text-align:right;"> 6.635296 </td>
   <td style="text-align:right;"> 0.2574223 </td>
   <td style="text-align:right;"> 47.14021 </td>
   <td style="text-align:right;"> 6.117470 </td>
   <td style="text-align:right;"> 7.153123 </td>
   <td style="text-align:left;"> ABCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> G129 </td>
   <td style="text-align:right;"> 6.683296 </td>
   <td style="text-align:right;"> 0.2583801 </td>
   <td style="text-align:right;"> 47.12376 </td>
   <td style="text-align:right;"> 6.163538 </td>
   <td style="text-align:right;"> 7.203053 </td>
   <td style="text-align:left;"> ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> G119 </td>
   <td style="text-align:right;"> 6.723201 </td>
   <td style="text-align:right;"> 0.2185871 </td>
   <td style="text-align:right;"> 57.00605 </td>
   <td style="text-align:right;"> 6.285489 </td>
   <td style="text-align:right;"> 7.160913 </td>
   <td style="text-align:left;"> BCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> G117 </td>
   <td style="text-align:right;"> 6.745757 </td>
   <td style="text-align:right;"> 0.2185525 </td>
   <td style="text-align:right;"> 56.97594 </td>
   <td style="text-align:right;"> 6.308109 </td>
   <td style="text-align:right;"> 7.183405 </td>
   <td style="text-align:left;"> BCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> G095 </td>
   <td style="text-align:right;"> 6.750903 </td>
   <td style="text-align:right;"> 0.2179454 </td>
   <td style="text-align:right;"> 58.94451 </td>
   <td style="text-align:right;"> 6.314787 </td>
   <td style="text-align:right;"> 7.187020 </td>
   <td style="text-align:left;"> BCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> G125 </td>
   <td style="text-align:right;"> 6.765862 </td>
   <td style="text-align:right;"> 0.2190818 </td>
   <td style="text-align:right;"> 57.55629 </td>
   <td style="text-align:right;"> 6.327250 </td>
   <td style="text-align:right;"> 7.204474 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> G113 </td>
   <td style="text-align:right;"> 6.782199 </td>
   <td style="text-align:right;"> 0.2596189 </td>
   <td style="text-align:right;"> 48.11522 </td>
   <td style="text-align:right;"> 6.260232 </td>
   <td style="text-align:right;"> 7.304165 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> G106 </td>
   <td style="text-align:right;"> 6.812462 </td>
   <td style="text-align:right;"> 0.2186371 </td>
   <td style="text-align:right;"> 57.05368 </td>
   <td style="text-align:right;"> 6.374658 </td>
   <td style="text-align:right;"> 7.250266 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> G100 </td>
   <td style="text-align:right;"> 6.839272 </td>
   <td style="text-align:right;"> 0.2173308 </td>
   <td style="text-align:right;"> 58.49435 </td>
   <td style="text-align:right;"> 6.404315 </td>
   <td style="text-align:right;"> 7.274229 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> G130 </td>
   <td style="text-align:right;"> 6.851351 </td>
   <td style="text-align:right;"> 0.2181168 </td>
   <td style="text-align:right;"> 56.52116 </td>
   <td style="text-align:right;"> 6.414499 </td>
   <td style="text-align:right;"> 7.288202 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> G133 </td>
   <td style="text-align:right;"> 6.860296 </td>
   <td style="text-align:right;"> 0.2574223 </td>
   <td style="text-align:right;"> 47.14021 </td>
   <td style="text-align:right;"> 6.342470 </td>
   <td style="text-align:right;"> 7.378122 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> G099 </td>
   <td style="text-align:right;"> 6.885296 </td>
   <td style="text-align:right;"> 0.2574223 </td>
   <td style="text-align:right;"> 47.14021 </td>
   <td style="text-align:right;"> 6.367470 </td>
   <td style="text-align:right;"> 7.403123 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> G122 </td>
   <td style="text-align:right;"> 6.888851 </td>
   <td style="text-align:right;"> 0.2181168 </td>
   <td style="text-align:right;"> 56.52116 </td>
   <td style="text-align:right;"> 6.452000 </td>
   <td style="text-align:right;"> 7.325702 </td>
   <td style="text-align:left;"> CDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> G108 </td>
   <td style="text-align:right;"> 6.890735 </td>
   <td style="text-align:right;"> 0.2596953 </td>
   <td style="text-align:right;"> 48.15288 </td>
   <td style="text-align:right;"> 6.368626 </td>
   <td style="text-align:right;"> 7.412845 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> G137 </td>
   <td style="text-align:right;"> 6.893143 </td>
   <td style="text-align:right;"> 0.2583178 </td>
   <td style="text-align:right;"> 47.10444 </td>
   <td style="text-align:right;"> 6.373505 </td>
   <td style="text-align:right;"> 7.412781 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> G112 </td>
   <td style="text-align:right;"> 6.895018 </td>
   <td style="text-align:right;"> 0.2588666 </td>
   <td style="text-align:right;"> 48.21872 </td>
   <td style="text-align:right;"> 6.374593 </td>
   <td style="text-align:right;"> 7.415443 </td>
   <td style="text-align:left;"> BCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> G127 </td>
   <td style="text-align:right;"> 6.914196 </td>
   <td style="text-align:right;"> 0.2190284 </td>
   <td style="text-align:right;"> 57.50217 </td>
   <td style="text-align:right;"> 6.475682 </td>
   <td style="text-align:right;"> 7.352710 </td>
   <td style="text-align:left;"> CDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> G107 </td>
   <td style="text-align:right;"> 6.951615 </td>
   <td style="text-align:right;"> 0.2186371 </td>
   <td style="text-align:right;"> 57.05368 </td>
   <td style="text-align:right;"> 6.513811 </td>
   <td style="text-align:right;"> 7.389420 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> G115 </td>
   <td style="text-align:right;"> 6.953993 </td>
   <td style="text-align:right;"> 0.2190398 </td>
   <td style="text-align:right;"> 57.49831 </td>
   <td style="text-align:right;"> 6.515456 </td>
   <td style="text-align:right;"> 7.392531 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> Hay </td>
   <td style="text-align:right;"> 6.954694 </td>
   <td style="text-align:right;"> 0.2182325 </td>
   <td style="text-align:right;"> 56.62938 </td>
   <td style="text-align:right;"> 6.517629 </td>
   <td style="text-align:right;"> 7.391759 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> G135 </td>
   <td style="text-align:right;"> 7.003554 </td>
   <td style="text-align:right;"> 0.2588419 </td>
   <td style="text-align:right;"> 48.22632 </td>
   <td style="text-align:right;"> 6.483181 </td>
   <td style="text-align:right;"> 7.523928 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> G124 </td>
   <td style="text-align:right;"> 7.004067 </td>
   <td style="text-align:right;"> 0.2583764 </td>
   <td style="text-align:right;"> 47.12366 </td>
   <td style="text-align:right;"> 6.484317 </td>
   <td style="text-align:right;"> 7.523817 </td>
   <td style="text-align:left;"> CDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> G126 </td>
   <td style="text-align:right;"> 7.005765 </td>
   <td style="text-align:right;"> 0.2181056 </td>
   <td style="text-align:right;"> 56.52038 </td>
   <td style="text-align:right;"> 6.568935 </td>
   <td style="text-align:right;"> 7.442594 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> G139 </td>
   <td style="text-align:right;"> 7.010296 </td>
   <td style="text-align:right;"> 0.2574223 </td>
   <td style="text-align:right;"> 47.14021 </td>
   <td style="text-align:right;"> 6.492470 </td>
   <td style="text-align:right;"> 7.528123 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> G142 </td>
   <td style="text-align:right;"> 7.013657 </td>
   <td style="text-align:right;"> 0.2597642 </td>
   <td style="text-align:right;"> 48.18062 </td>
   <td style="text-align:right;"> 6.491417 </td>
   <td style="text-align:right;"> 7.535898 </td>
   <td style="text-align:left;"> CDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> G102 </td>
   <td style="text-align:right;"> 7.033902 </td>
   <td style="text-align:right;"> 0.2189640 </td>
   <td style="text-align:right;"> 57.42904 </td>
   <td style="text-align:right;"> 6.595505 </td>
   <td style="text-align:right;"> 7.472299 </td>
   <td style="text-align:left;"> FG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> G140 </td>
   <td style="text-align:right;"> 7.034964 </td>
   <td style="text-align:right;"> 0.2168805 </td>
   <td style="text-align:right;"> 57.95499 </td>
   <td style="text-align:right;"> 6.600824 </td>
   <td style="text-align:right;"> 7.469105 </td>
   <td style="text-align:left;"> EFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> G120 </td>
   <td style="text-align:right;"> 7.051717 </td>
   <td style="text-align:right;"> 0.2627172 </td>
   <td style="text-align:right;"> 44.95573 </td>
   <td style="text-align:right;"> 6.522563 </td>
   <td style="text-align:right;"> 7.580871 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> V002 </td>
   <td style="text-align:right;"> 7.052308 </td>
   <td style="text-align:right;"> 0.2185206 </td>
   <td style="text-align:right;"> 56.94105 </td>
   <td style="text-align:right;"> 6.614718 </td>
   <td style="text-align:right;"> 7.489898 </td>
   <td style="text-align:left;"> FG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> G128 </td>
   <td style="text-align:right;"> 7.064939 </td>
   <td style="text-align:right;"> 0.2597642 </td>
   <td style="text-align:right;"> 48.18062 </td>
   <td style="text-align:right;"> 6.542699 </td>
   <td style="text-align:right;"> 7.587180 </td>
   <td style="text-align:left;"> DEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> G109 </td>
   <td style="text-align:right;"> 7.078221 </td>
   <td style="text-align:right;"> 0.2190446 </td>
   <td style="text-align:right;"> 57.50198 </td>
   <td style="text-align:right;"> 6.639675 </td>
   <td style="text-align:right;"> 7.516767 </td>
   <td style="text-align:left;"> FG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> G011 </td>
   <td style="text-align:right;"> 7.189772 </td>
   <td style="text-align:right;"> 0.2177726 </td>
   <td style="text-align:right;"> 55.84364 </td>
   <td style="text-align:right;"> 6.753494 </td>
   <td style="text-align:right;"> 7.626050 </td>
   <td style="text-align:left;"> G </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> G136 </td>
   <td style="text-align:right;"> 7.216822 </td>
   <td style="text-align:right;"> 0.2627093 </td>
   <td style="text-align:right;"> 44.96037 </td>
   <td style="text-align:right;"> 6.687685 </td>
   <td style="text-align:right;"> 7.745959 </td>
   <td style="text-align:left;"> FGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> G145 </td>
   <td style="text-align:right;"> 7.745126 </td>
   <td style="text-align:right;"> 0.2597413 </td>
   <td style="text-align:right;"> 48.17317 </td>
   <td style="text-align:right;"> 7.222930 </td>
   <td style="text-align:right;"> 8.267323 </td>
   <td style="text-align:left;"> H </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> contrast </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> t.ratio </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> G011 - Hay </td>
   <td style="text-align:right;"> 0.2350776 </td>
   <td style="text-align:right;"> 0.2041960 </td>
   <td style="text-align:right;"> 18.70037 </td>
   <td style="text-align:right;"> 1.1512354 </td>
   <td style="text-align:right;"> 0.2641429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G095 - Hay </td>
   <td style="text-align:right;"> -0.2037907 </td>
   <td style="text-align:right;"> 0.1947013 </td>
   <td style="text-align:right;"> 19.47759 </td>
   <td style="text-align:right;"> -1.0466836 </td>
   <td style="text-align:right;"> 0.3080615 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G099 - Hay </td>
   <td style="text-align:right;"> -0.0693978 </td>
   <td style="text-align:right;"> 0.2296267 </td>
   <td style="text-align:right;"> 17.60882 </td>
   <td style="text-align:right;"> -0.3022201 </td>
   <td style="text-align:right;"> 0.7660249 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G100 - Hay </td>
   <td style="text-align:right;"> -0.1154221 </td>
   <td style="text-align:right;"> 0.1949988 </td>
   <td style="text-align:right;"> 19.65762 </td>
   <td style="text-align:right;"> -0.5919120 </td>
   <td style="text-align:right;"> 0.5606563 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G102 - Hay </td>
   <td style="text-align:right;"> 0.0792082 </td>
   <td style="text-align:right;"> 0.1848317 </td>
   <td style="text-align:right;"> 17.64789 </td>
   <td style="text-align:right;"> 0.4285425 </td>
   <td style="text-align:right;"> 0.6734411 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G106 - Hay </td>
   <td style="text-align:right;"> -0.1422321 </td>
   <td style="text-align:right;"> 0.1842827 </td>
   <td style="text-align:right;"> 17.43692 </td>
   <td style="text-align:right;"> -0.7718147 </td>
   <td style="text-align:right;"> 0.4505584 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G107 - Hay </td>
   <td style="text-align:right;"> -0.0030788 </td>
   <td style="text-align:right;"> 0.1842827 </td>
   <td style="text-align:right;"> 17.43692 </td>
   <td style="text-align:right;"> -0.0167072 </td>
   <td style="text-align:right;"> 0.9868599 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G108 - Hay </td>
   <td style="text-align:right;"> -0.0639589 </td>
   <td style="text-align:right;"> 0.2311026 </td>
   <td style="text-align:right;"> 18.07550 </td>
   <td style="text-align:right;"> -0.2767554 </td>
   <td style="text-align:right;"> 0.7851079 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G109 - Hay </td>
   <td style="text-align:right;"> 0.1235268 </td>
   <td style="text-align:right;"> 0.1847877 </td>
   <td style="text-align:right;"> 17.63128 </td>
   <td style="text-align:right;"> 0.6684795 </td>
   <td style="text-align:right;"> 0.5124851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G112 - Hay </td>
   <td style="text-align:right;"> -0.0596760 </td>
   <td style="text-align:right;"> 0.2312576 </td>
   <td style="text-align:right;"> 18.11764 </td>
   <td style="text-align:right;"> -0.2580497 </td>
   <td style="text-align:right;"> 0.7992770 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G113 - Hay </td>
   <td style="text-align:right;"> -0.1724952 </td>
   <td style="text-align:right;"> 0.2311416 </td>
   <td style="text-align:right;"> 18.09456 </td>
   <td style="text-align:right;"> -0.7462750 </td>
   <td style="text-align:right;"> 0.4650874 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G115 - Hay </td>
   <td style="text-align:right;"> -0.0007010 </td>
   <td style="text-align:right;"> 0.1849994 </td>
   <td style="text-align:right;"> 17.71137 </td>
   <td style="text-align:right;"> -0.0037891 </td>
   <td style="text-align:right;"> 0.9970191 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G117 - Hay </td>
   <td style="text-align:right;"> -0.2089370 </td>
   <td style="text-align:right;"> 0.1844187 </td>
   <td style="text-align:right;"> 17.48813 </td>
   <td style="text-align:right;"> -1.1329487 </td>
   <td style="text-align:right;"> 0.2725301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G118 - Hay </td>
   <td style="text-align:right;"> -0.4965308 </td>
   <td style="text-align:right;"> 0.1943798 </td>
   <td style="text-align:right;"> 19.26282 </td>
   <td style="text-align:right;"> -2.5544361 </td>
   <td style="text-align:right;"> 0.0192444 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G119 - Hay </td>
   <td style="text-align:right;"> -0.2314933 </td>
   <td style="text-align:right;"> 0.1843268 </td>
   <td style="text-align:right;"> 17.45311 </td>
   <td style="text-align:right;"> -1.2558853 </td>
   <td style="text-align:right;"> 0.2257104 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G120 - Hay </td>
   <td style="text-align:right;"> 0.0970228 </td>
   <td style="text-align:right;"> 0.2530315 </td>
   <td style="text-align:right;"> 19.28589 </td>
   <td style="text-align:right;"> 0.3834416 </td>
   <td style="text-align:right;"> 0.7055865 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G121 - Hay </td>
   <td style="text-align:right;"> -0.4315760 </td>
   <td style="text-align:right;"> 0.2296558 </td>
   <td style="text-align:right;"> 17.61447 </td>
   <td style="text-align:right;"> -1.8792299 </td>
   <td style="text-align:right;"> 0.0768624 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G122 - Hay </td>
   <td style="text-align:right;"> -0.0658432 </td>
   <td style="text-align:right;"> 0.1838819 </td>
   <td style="text-align:right;"> 17.28779 </td>
   <td style="text-align:right;"> -0.3580732 </td>
   <td style="text-align:right;"> 0.7246233 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G123 - Hay </td>
   <td style="text-align:right;"> -0.4213984 </td>
   <td style="text-align:right;"> 0.2295663 </td>
   <td style="text-align:right;"> 17.58458 </td>
   <td style="text-align:right;"> -1.8356282 </td>
   <td style="text-align:right;"> 0.0833829 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G124 - Hay </td>
   <td style="text-align:right;"> 0.0493728 </td>
   <td style="text-align:right;"> 0.2295202 </td>
   <td style="text-align:right;"> 17.57157 </td>
   <td style="text-align:right;"> 0.2151132 </td>
   <td style="text-align:right;"> 0.8321560 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G125 - Hay </td>
   <td style="text-align:right;"> -0.1888319 </td>
   <td style="text-align:right;"> 0.1848483 </td>
   <td style="text-align:right;"> 17.65427 </td>
   <td style="text-align:right;"> -1.0215507 </td>
   <td style="text-align:right;"> 0.3207891 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G126 - Hay </td>
   <td style="text-align:right;"> 0.0510704 </td>
   <td style="text-align:right;"> 0.1840754 </td>
   <td style="text-align:right;"> 17.35946 </td>
   <td style="text-align:right;"> 0.2774429 </td>
   <td style="text-align:right;"> 0.7847170 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G127 - Hay </td>
   <td style="text-align:right;"> -0.0404983 </td>
   <td style="text-align:right;"> 0.1848074 </td>
   <td style="text-align:right;"> 17.63876 </td>
   <td style="text-align:right;"> -0.2191376 </td>
   <td style="text-align:right;"> 0.8290586 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G128 - Hay </td>
   <td style="text-align:right;"> 0.1102451 </td>
   <td style="text-align:right;"> 0.2311179 </td>
   <td style="text-align:right;"> 18.07641 </td>
   <td style="text-align:right;"> 0.4770081 </td>
   <td style="text-align:right;"> 0.6390724 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G129 - Hay </td>
   <td style="text-align:right;"> -0.2713984 </td>
   <td style="text-align:right;"> 0.2295663 </td>
   <td style="text-align:right;"> 17.58458 </td>
   <td style="text-align:right;"> -1.1822223 </td>
   <td style="text-align:right;"> 0.2528577 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G130 - Hay </td>
   <td style="text-align:right;"> -0.1033432 </td>
   <td style="text-align:right;"> 0.1838819 </td>
   <td style="text-align:right;"> 17.28779 </td>
   <td style="text-align:right;"> -0.5620084 </td>
   <td style="text-align:right;"> 0.5813245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G131 - Hay </td>
   <td style="text-align:right;"> -0.6609195 </td>
   <td style="text-align:right;"> 0.1838860 </td>
   <td style="text-align:right;"> 17.28917 </td>
   <td style="text-align:right;"> -3.5941808 </td>
   <td style="text-align:right;"> 0.0021871 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G133 - Hay </td>
   <td style="text-align:right;"> -0.0943978 </td>
   <td style="text-align:right;"> 0.2296267 </td>
   <td style="text-align:right;"> 17.60882 </td>
   <td style="text-align:right;"> -0.4110925 </td>
   <td style="text-align:right;"> 0.6859645 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G135 - Hay </td>
   <td style="text-align:right;"> 0.0488604 </td>
   <td style="text-align:right;"> 0.2311053 </td>
   <td style="text-align:right;"> 18.07625 </td>
   <td style="text-align:right;"> 0.2114204 </td>
   <td style="text-align:right;"> 0.8349246 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G136 - Hay </td>
   <td style="text-align:right;"> 0.2621278 </td>
   <td style="text-align:right;"> 0.2530204 </td>
   <td style="text-align:right;"> 19.28671 </td>
   <td style="text-align:right;"> 1.0359948 </td>
   <td style="text-align:right;"> 0.3130202 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G137 - Hay </td>
   <td style="text-align:right;"> -0.0615510 </td>
   <td style="text-align:right;"> 0.2294784 </td>
   <td style="text-align:right;"> 17.56312 </td>
   <td style="text-align:right;"> -0.2682215 </td>
   <td style="text-align:right;"> 0.7916537 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G139 - Hay </td>
   <td style="text-align:right;"> 0.0556022 </td>
   <td style="text-align:right;"> 0.2296267 </td>
   <td style="text-align:right;"> 17.60882 </td>
   <td style="text-align:right;"> 0.2421418 </td>
   <td style="text-align:right;"> 0.8114680 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G140 - Hay </td>
   <td style="text-align:right;"> 0.0802703 </td>
   <td style="text-align:right;"> 0.1945893 </td>
   <td style="text-align:right;"> 19.47805 </td>
   <td style="text-align:right;"> 0.4125113 </td>
   <td style="text-align:right;"> 0.6844697 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G142 - Hay </td>
   <td style="text-align:right;"> 0.0589630 </td>
   <td style="text-align:right;"> 0.2311179 </td>
   <td style="text-align:right;"> 18.07641 </td>
   <td style="text-align:right;"> 0.2551211 </td>
   <td style="text-align:right;"> 0.8015097 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G143 - Hay </td>
   <td style="text-align:right;"> -0.5129011 </td>
   <td style="text-align:right;"> 0.2310243 </td>
   <td style="text-align:right;"> 18.05288 </td>
   <td style="text-align:right;"> -2.2201171 </td>
   <td style="text-align:right;"> 0.0394506 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G145 - Hay </td>
   <td style="text-align:right;"> 0.7904319 </td>
   <td style="text-align:right;"> 0.2311042 </td>
   <td style="text-align:right;"> 18.07375 </td>
   <td style="text-align:right;"> 3.4202409 </td>
   <td style="text-align:right;"> 0.0030373 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G149 - Hay </td>
   <td style="text-align:right;"> -0.3193978 </td>
   <td style="text-align:right;"> 0.2296267 </td>
   <td style="text-align:right;"> 17.60882 </td>
   <td style="text-align:right;"> -1.3909439 </td>
   <td style="text-align:right;"> 0.1815704 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V002 - Hay </td>
   <td style="text-align:right;"> 0.0976138 </td>
   <td style="text-align:right;"> 0.1843638 </td>
   <td style="text-align:right;"> 17.46795 </td>
   <td style="text-align:right;"> 0.5294626 </td>
   <td style="text-align:right;"> 0.6031492 </td>
  </tr>
</tbody>
</table>

Now look at the means by the selection level i.e. Liking by Cultivar x Site x Graft year

```
## Warning: 'CLD' will be deprecated. Its use is discouraged.
## See '? CLD' for an explanation. Use 'pwpp' or 'multcomp::cld' instead.
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> selections </th>
   <th style="text-align:right;"> emmean </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> lower.CL </th>
   <th style="text-align:right;"> upper.CL </th>
   <th style="text-align:left;"> .group </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> G118.16.TRC </td>
   <td style="text-align:right;"> 4.638719 </td>
   <td style="text-align:right;"> 0.2831062 </td>
   <td style="text-align:right;"> 378.4129 </td>
   <td style="text-align:right;"> 4.082061 </td>
   <td style="text-align:right;"> 5.195378 </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> G118.16.HGT </td>
   <td style="text-align:right;"> 5.118399 </td>
   <td style="text-align:right;"> 0.2856296 </td>
   <td style="text-align:right;"> 390.4390 </td>
   <td style="text-align:right;"> 4.556834 </td>
   <td style="text-align:right;"> 5.679963 </td>
   <td style="text-align:left;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> G133.17.TRC </td>
   <td style="text-align:right;"> 5.323757 </td>
   <td style="text-align:right;"> 0.2830127 </td>
   <td style="text-align:right;"> 378.2086 </td>
   <td style="text-align:right;"> 4.767281 </td>
   <td style="text-align:right;"> 5.880232 </td>
   <td style="text-align:left;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> G143.17.TRC </td>
   <td style="text-align:right;"> 5.558615 </td>
   <td style="text-align:right;"> 0.2856320 </td>
   <td style="text-align:right;"> 390.4437 </td>
   <td style="text-align:right;"> 4.997046 </td>
   <td style="text-align:right;"> 6.120185 </td>
   <td style="text-align:left;"> 234 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> G122.16.TRC </td>
   <td style="text-align:right;"> 5.638719 </td>
   <td style="text-align:right;"> 0.2831062 </td>
   <td style="text-align:right;"> 378.4129 </td>
   <td style="text-align:right;"> 5.082061 </td>
   <td style="text-align:right;"> 6.195378 </td>
   <td style="text-align:left;"> 2345 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> G126.16.TRC </td>
   <td style="text-align:right;"> 5.683281 </td>
   <td style="text-align:right;"> 0.2829258 </td>
   <td style="text-align:right;"> 378.1788 </td>
   <td style="text-align:right;"> 5.126976 </td>
   <td style="text-align:right;"> 6.239585 </td>
   <td style="text-align:left;"> 23456 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> G149.17.TRC </td>
   <td style="text-align:right;"> 5.698757 </td>
   <td style="text-align:right;"> 0.2830127 </td>
   <td style="text-align:right;"> 378.2086 </td>
   <td style="text-align:right;"> 5.142281 </td>
   <td style="text-align:right;"> 6.255232 </td>
   <td style="text-align:left;"> 234567 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> G131.16.TRC </td>
   <td style="text-align:right;"> 5.724904 </td>
   <td style="text-align:right;"> 0.2830970 </td>
   <td style="text-align:right;"> 378.3886 </td>
   <td style="text-align:right;"> 5.168264 </td>
   <td style="text-align:right;"> 6.281544 </td>
   <td style="text-align:left;"> 234567 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> G125.16.TRC </td>
   <td style="text-align:right;"> 5.735333 </td>
   <td style="text-align:right;"> 0.2856177 </td>
   <td style="text-align:right;"> 390.3953 </td>
   <td style="text-align:right;"> 5.173792 </td>
   <td style="text-align:right;"> 6.296874 </td>
   <td style="text-align:left;"> 2345678 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> Hay.141617.TRC </td>
   <td style="text-align:right;"> 5.774904 </td>
   <td style="text-align:right;"> 0.2830970 </td>
   <td style="text-align:right;"> 378.3886 </td>
   <td style="text-align:right;"> 5.218263 </td>
   <td style="text-align:right;"> 6.331544 </td>
   <td style="text-align:left;"> 3456789 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> G112.16.TRC </td>
   <td style="text-align:right;"> 5.811841 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 5.250448 </td>
   <td style="text-align:right;"> 6.373234 </td>
   <td style="text-align:left;"> 34567890 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> G011.1314.HGT </td>
   <td style="text-align:right;"> 5.845915 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 5.284340 </td>
   <td style="text-align:right;"> 6.407490 </td>
   <td style="text-align:left;"> 34567890A </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> G099.15.TRC </td>
   <td style="text-align:right;"> 5.848757 </td>
   <td style="text-align:right;"> 0.2830127 </td>
   <td style="text-align:right;"> 378.2086 </td>
   <td style="text-align:right;"> 5.292281 </td>
   <td style="text-align:right;"> 6.405232 </td>
   <td style="text-align:left;"> 34567890A </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> G121.16.WGT </td>
   <td style="text-align:right;"> 5.873756 </td>
   <td style="text-align:right;"> 0.2830127 </td>
   <td style="text-align:right;"> 378.2086 </td>
   <td style="text-align:right;"> 5.317281 </td>
   <td style="text-align:right;"> 6.430232 </td>
   <td style="text-align:left;"> 34567890A </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> G139.17.TRC </td>
   <td style="text-align:right;"> 6.023757 </td>
   <td style="text-align:right;"> 0.2830127 </td>
   <td style="text-align:right;"> 378.2086 </td>
   <td style="text-align:right;"> 5.467281 </td>
   <td style="text-align:right;"> 6.580232 </td>
   <td style="text-align:left;"> 4567890AB </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> G131.16.WGT </td>
   <td style="text-align:right;"> 6.033281 </td>
   <td style="text-align:right;"> 0.2829258 </td>
   <td style="text-align:right;"> 378.1788 </td>
   <td style="text-align:right;"> 5.476976 </td>
   <td style="text-align:right;"> 6.589585 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> G117.16.TRC </td>
   <td style="text-align:right;"> 6.048756 </td>
   <td style="text-align:right;"> 0.2830127 </td>
   <td style="text-align:right;"> 378.2086 </td>
   <td style="text-align:right;"> 5.492281 </td>
   <td style="text-align:right;"> 6.605232 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> G100.15.KRC </td>
   <td style="text-align:right;"> 6.051043 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 5.489468 </td>
   <td style="text-align:right;"> 6.612619 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> G115.16.TRC </td>
   <td style="text-align:right;"> 6.068251 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 5.506858 </td>
   <td style="text-align:right;"> 6.629644 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> G107.15.TRC </td>
   <td style="text-align:right;"> 6.092758 </td>
   <td style="text-align:right;"> 0.2856296 </td>
   <td style="text-align:right;"> 390.4390 </td>
   <td style="text-align:right;"> 5.531193 </td>
   <td style="text-align:right;"> 6.654322 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> G106.15.WGT </td>
   <td style="text-align:right;"> 6.124904 </td>
   <td style="text-align:right;"> 0.2830970 </td>
   <td style="text-align:right;"> 378.3886 </td>
   <td style="text-align:right;"> 5.568263 </td>
   <td style="text-align:right;"> 6.681544 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> Hay.1416.WGT </td>
   <td style="text-align:right;"> 6.137771 </td>
   <td style="text-align:right;"> 0.2831075 </td>
   <td style="text-align:right;"> 378.4170 </td>
   <td style="text-align:right;"> 5.581110 </td>
   <td style="text-align:right;"> 6.694432 </td>
   <td style="text-align:left;"> 4567890ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> G113.16.WGT </td>
   <td style="text-align:right;"> 6.145174 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 5.583781 </td>
   <td style="text-align:right;"> 6.706567 </td>
   <td style="text-align:left;"> 4567890ABCD </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> G100.15.TRC </td>
   <td style="text-align:right;"> 6.204890 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 5.643314 </td>
   <td style="text-align:right;"> 6.766465 </td>
   <td style="text-align:left;"> 4567890ABCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> G109.15.WGT </td>
   <td style="text-align:right;"> 6.222513 </td>
   <td style="text-align:right;"> 0.2856177 </td>
   <td style="text-align:right;"> 390.3953 </td>
   <td style="text-align:right;"> 5.660971 </td>
   <td style="text-align:right;"> 6.784054 </td>
   <td style="text-align:left;"> 567890ABCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> G109.15.TRC </td>
   <td style="text-align:right;"> 6.230531 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 5.668955 </td>
   <td style="text-align:right;"> 6.792106 </td>
   <td style="text-align:left;"> 567890ABCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> G126.16.WGT </td>
   <td style="text-align:right;"> 6.238300 </td>
   <td style="text-align:right;"> 0.2829174 </td>
   <td style="text-align:right;"> 378.1492 </td>
   <td style="text-align:right;"> 5.682011 </td>
   <td style="text-align:right;"> 6.794588 </td>
   <td style="text-align:left;"> 567890ABCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> G095.1617.HGT </td>
   <td style="text-align:right;"> 6.256172 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 5.694596 </td>
   <td style="text-align:right;"> 6.817747 </td>
   <td style="text-align:left;"> 567890ABCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> G140.17.KRC </td>
   <td style="text-align:right;"> 6.262771 </td>
   <td style="text-align:right;"> 0.2831075 </td>
   <td style="text-align:right;"> 378.4170 </td>
   <td style="text-align:right;"> 5.706110 </td>
   <td style="text-align:right;"> 6.819432 </td>
   <td style="text-align:left;"> 567890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> G140.17.TRC </td>
   <td style="text-align:right;"> 6.273795 </td>
   <td style="text-align:right;"> 0.2856177 </td>
   <td style="text-align:right;"> 390.3953 </td>
   <td style="text-align:right;"> 5.712254 </td>
   <td style="text-align:right;"> 6.835336 </td>
   <td style="text-align:left;"> 567890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> G136.17.HGT </td>
   <td style="text-align:right;"> 6.283281 </td>
   <td style="text-align:right;"> 0.2829258 </td>
   <td style="text-align:right;"> 378.1788 </td>
   <td style="text-align:right;"> 5.726976 </td>
   <td style="text-align:right;"> 6.839585 </td>
   <td style="text-align:left;"> 567890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> G102.15.TRC </td>
   <td style="text-align:right;"> 6.284904 </td>
   <td style="text-align:right;"> 0.2856049 </td>
   <td style="text-align:right;"> 390.3490 </td>
   <td style="text-align:right;"> 5.723388 </td>
   <td style="text-align:right;"> 6.846421 </td>
   <td style="text-align:left;"> 567890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> G106.15.TRC </td>
   <td style="text-align:right;"> 6.297886 </td>
   <td style="text-align:right;"> 0.2856296 </td>
   <td style="text-align:right;"> 390.4390 </td>
   <td style="text-align:right;"> 5.736321 </td>
   <td style="text-align:right;"> 6.859450 </td>
   <td style="text-align:left;"> 67890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> G137.17.WGT </td>
   <td style="text-align:right;"> 6.324904 </td>
   <td style="text-align:right;"> 0.2830970 </td>
   <td style="text-align:right;"> 378.3886 </td>
   <td style="text-align:right;"> 5.768263 </td>
   <td style="text-align:right;"> 6.881544 </td>
   <td style="text-align:left;"> 67890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> V002.16.TRC </td>
   <td style="text-align:right;"> 6.349168 </td>
   <td style="text-align:right;"> 0.2856296 </td>
   <td style="text-align:right;"> 390.4390 </td>
   <td style="text-align:right;"> 5.787603 </td>
   <td style="text-align:right;"> 6.910732 </td>
   <td style="text-align:left;"> 7890ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> G119.16.TRC </td>
   <td style="text-align:right;"> 6.387771 </td>
   <td style="text-align:right;"> 0.2831075 </td>
   <td style="text-align:right;"> 378.4170 </td>
   <td style="text-align:right;"> 5.831110 </td>
   <td style="text-align:right;"> 6.944432 </td>
   <td style="text-align:left;"> 890ABCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> G122.16.WGT </td>
   <td style="text-align:right;"> 6.403515 </td>
   <td style="text-align:right;"> 0.2854532 </td>
   <td style="text-align:right;"> 390.2159 </td>
   <td style="text-align:right;"> 5.842297 </td>
   <td style="text-align:right;"> 6.964734 </td>
   <td style="text-align:left;"> 90ABCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> G135.17.TRC </td>
   <td style="text-align:right;"> 6.438751 </td>
   <td style="text-align:right;"> 0.2856049 </td>
   <td style="text-align:right;"> 390.3490 </td>
   <td style="text-align:right;"> 5.877234 </td>
   <td style="text-align:right;"> 7.000267 </td>
   <td style="text-align:left;"> 0ABCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> G115.16.WGT </td>
   <td style="text-align:right;"> 6.478507 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 5.917115 </td>
   <td style="text-align:right;"> 7.039900 </td>
   <td style="text-align:left;"> ABCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> G129.16.WGT </td>
   <td style="text-align:right;"> 6.488719 </td>
   <td style="text-align:right;"> 0.2831062 </td>
   <td style="text-align:right;"> 378.4129 </td>
   <td style="text-align:right;"> 5.932061 </td>
   <td style="text-align:right;"> 7.045378 </td>
   <td style="text-align:left;"> ABCDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> G128.16.WGT </td>
   <td style="text-align:right;"> 6.538223 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 5.976648 </td>
   <td style="text-align:right;"> 7.099798 </td>
   <td style="text-align:left;"> BCDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> G117.16.WGT </td>
   <td style="text-align:right;"> 6.581072 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 6.019679 </td>
   <td style="text-align:right;"> 7.142464 </td>
   <td style="text-align:left;"> BCDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> G119.16.WGT </td>
   <td style="text-align:right;"> 6.606713 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 6.045320 </td>
   <td style="text-align:right;"> 7.168105 </td>
   <td style="text-align:left;"> BCDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> G120.16.HGT </td>
   <td style="text-align:right;"> 6.613300 </td>
   <td style="text-align:right;"> 0.2829174 </td>
   <td style="text-align:right;"> 378.1492 </td>
   <td style="text-align:right;"> 6.057011 </td>
   <td style="text-align:right;"> 7.169588 </td>
   <td style="text-align:left;"> BCDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> G127.16.TRC </td>
   <td style="text-align:right;"> 6.658410 </td>
   <td style="text-align:right;"> 0.2856177 </td>
   <td style="text-align:right;"> 390.3953 </td>
   <td style="text-align:right;"> 6.096869 </td>
   <td style="text-align:right;"> 7.219951 </td>
   <td style="text-align:left;"> BCDEFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> G123.16.WGT </td>
   <td style="text-align:right;"> 6.663719 </td>
   <td style="text-align:right;"> 0.2831062 </td>
   <td style="text-align:right;"> 378.4129 </td>
   <td style="text-align:right;"> 6.107061 </td>
   <td style="text-align:right;"> 7.220378 </td>
   <td style="text-align:left;"> BCDEFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> G130.16.WGT </td>
   <td style="text-align:right;"> 6.683281 </td>
   <td style="text-align:right;"> 0.2829258 </td>
   <td style="text-align:right;"> 378.1788 </td>
   <td style="text-align:right;"> 6.126976 </td>
   <td style="text-align:right;"> 7.239585 </td>
   <td style="text-align:left;"> CDEFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> G095.1617.WGT </td>
   <td style="text-align:right;"> 6.797725 </td>
   <td style="text-align:right;"> 0.2856049 </td>
   <td style="text-align:right;"> 390.3490 </td>
   <td style="text-align:right;"> 6.236209 </td>
   <td style="text-align:right;"> 7.359241 </td>
   <td style="text-align:left;"> DEFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> G125.16.WGT </td>
   <td style="text-align:right;"> 6.812256 </td>
   <td style="text-align:right;"> 0.2856177 </td>
   <td style="text-align:right;"> 390.3953 </td>
   <td style="text-align:right;"> 6.250715 </td>
   <td style="text-align:right;"> 7.373798 </td>
   <td style="text-align:left;"> EFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> G130.16.TRC </td>
   <td style="text-align:right;"> 6.813719 </td>
   <td style="text-align:right;"> 0.2831062 </td>
   <td style="text-align:right;"> 378.4129 </td>
   <td style="text-align:right;"> 6.257061 </td>
   <td style="text-align:right;"> 7.370378 </td>
   <td style="text-align:left;"> EFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> G142.17.WGT </td>
   <td style="text-align:right;"> 6.845915 </td>
   <td style="text-align:right;"> 0.2856352 </td>
   <td style="text-align:right;"> 390.4585 </td>
   <td style="text-align:right;"> 6.284340 </td>
   <td style="text-align:right;"> 7.407490 </td>
   <td style="text-align:left;"> EFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> G108.15.WGT </td>
   <td style="text-align:right;"> 6.849007 </td>
   <td style="text-align:right;"> 0.2856049 </td>
   <td style="text-align:right;"> 390.3490 </td>
   <td style="text-align:right;"> 6.287491 </td>
   <td style="text-align:right;"> 7.410523 </td>
   <td style="text-align:left;"> EFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> G107.15.WGT </td>
   <td style="text-align:right;"> 6.899904 </td>
   <td style="text-align:right;"> 0.2830970 </td>
   <td style="text-align:right;"> 378.3886 </td>
   <td style="text-align:right;"> 6.343263 </td>
   <td style="text-align:right;"> 7.456544 </td>
   <td style="text-align:left;"> FGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> G127.16.WGT </td>
   <td style="text-align:right;"> 6.900289 </td>
   <td style="text-align:right;"> 0.2856049 </td>
   <td style="text-align:right;"> 390.3490 </td>
   <td style="text-align:right;"> 6.338773 </td>
   <td style="text-align:right;"> 7.461805 </td>
   <td style="text-align:left;"> FGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> V002.16.WGT </td>
   <td style="text-align:right;"> 6.913299 </td>
   <td style="text-align:right;"> 0.2829174 </td>
   <td style="text-align:right;"> 378.1492 </td>
   <td style="text-align:right;"> 6.357011 </td>
   <td style="text-align:right;"> 7.469588 </td>
   <td style="text-align:left;"> GHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> G011.0814.GGT </td>
   <td style="text-align:right;"> 7.012771 </td>
   <td style="text-align:right;"> 0.2831075 </td>
   <td style="text-align:right;"> 378.4170 </td>
   <td style="text-align:right;"> 6.456110 </td>
   <td style="text-align:right;"> 7.569432 </td>
   <td style="text-align:left;"> HIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> G102.15.WGT </td>
   <td style="text-align:right;"> 7.016969 </td>
   <td style="text-align:right;"> 0.2855419 </td>
   <td style="text-align:right;"> 390.2574 </td>
   <td style="text-align:right;"> 6.455576 </td>
   <td style="text-align:right;"> 7.578362 </td>
   <td style="text-align:left;"> HIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> G124.16.WGT </td>
   <td style="text-align:right;"> 7.137771 </td>
   <td style="text-align:right;"> 0.2831075 </td>
   <td style="text-align:right;"> 378.4170 </td>
   <td style="text-align:right;"> 6.581110 </td>
   <td style="text-align:right;"> 7.694432 </td>
   <td style="text-align:left;"> IJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> G145.17.WGT </td>
   <td style="text-align:right;"> 7.272245 </td>
   <td style="text-align:right;"> 0.2856296 </td>
   <td style="text-align:right;"> 390.4390 </td>
   <td style="text-align:right;"> 6.710680 </td>
   <td style="text-align:right;"> 7.833809 </td>
   <td style="text-align:left;"> J </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> contrast </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> t.ratio </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> G011.0814.GGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.2378671 </td>
   <td style="text-align:right;"> 0.3320525 </td>
   <td style="text-align:right;"> 2227.322 </td>
   <td style="text-align:right;"> 3.7279255 </td>
   <td style="text-align:right;"> 0.0001979 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G011.1314.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0710113 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 0.2124715 </td>
   <td style="text-align:right;"> 0.8317586 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G095.1617.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4812678 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 1.4399909 </td>
   <td style="text-align:right;"> 0.1500105 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G095.1617.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.0228212 </td>
   <td style="text-align:right;"> 0.3341838 </td>
   <td style="text-align:right;"> 2227.287 </td>
   <td style="text-align:right;"> 3.0606550 </td>
   <td style="text-align:right;"> 0.0022349 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G099.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0738526 </td>
   <td style="text-align:right;"> 0.3321438 </td>
   <td style="text-align:right;"> 2227.783 </td>
   <td style="text-align:right;"> 0.2223514 </td>
   <td style="text-align:right;"> 0.8240607 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G100.15.KRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2761395 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 0.8262312 </td>
   <td style="text-align:right;"> 0.4087614 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G100.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4299857 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 1.2865510 </td>
   <td style="text-align:right;"> 0.1983845 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G102.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.5100007 </td>
   <td style="text-align:right;"> 0.3341838 </td>
   <td style="text-align:right;"> 2227.287 </td>
   <td style="text-align:right;"> 1.5261086 </td>
   <td style="text-align:right;"> 0.1271248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G102.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.2420652 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 3.7153724 </td>
   <td style="text-align:right;"> 0.0002079 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G106.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.5229819 </td>
   <td style="text-align:right;"> 0.3342095 </td>
   <td style="text-align:right;"> 2227.346 </td>
   <td style="text-align:right;"> 1.5648328 </td>
   <td style="text-align:right;"> 0.1177642 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G106.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3500000 </td>
   <td style="text-align:right;"> 0.3318484 </td>
   <td style="text-align:right;"> 2227.040 </td>
   <td style="text-align:right;"> 1.0546986 </td>
   <td style="text-align:right;"> 0.2916776 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G107.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3178537 </td>
   <td style="text-align:right;"> 0.3342095 </td>
   <td style="text-align:right;"> 2227.346 </td>
   <td style="text-align:right;"> 0.9510614 </td>
   <td style="text-align:right;"> 0.3416764 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G107.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.1250000 </td>
   <td style="text-align:right;"> 0.3318484 </td>
   <td style="text-align:right;"> 2227.040 </td>
   <td style="text-align:right;"> 3.3901026 </td>
   <td style="text-align:right;"> 0.0007108 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G108.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.0741033 </td>
   <td style="text-align:right;"> 0.3341838 </td>
   <td style="text-align:right;"> 2227.287 </td>
   <td style="text-align:right;"> 3.2141097 </td>
   <td style="text-align:right;"> 0.0013272 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G109.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4556267 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 1.3632710 </td>
   <td style="text-align:right;"> 0.1729349 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G109.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4476089 </td>
   <td style="text-align:right;"> 0.3341014 </td>
   <td style="text-align:right;"> 2227.197 </td>
   <td style="text-align:right;"> 1.3397395 </td>
   <td style="text-align:right;"> 0.1804667 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G112.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0369370 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 0.1104890 </td>
   <td style="text-align:right;"> 0.9120315 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G113.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3702703 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 1.1075844 </td>
   <td style="text-align:right;"> 0.2681609 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G115.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2933472 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 0.8774855 </td>
   <td style="text-align:right;"> 0.3803176 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G115.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.7036036 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 2.1046798 </td>
   <td style="text-align:right;"> 0.0354309 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G117.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2738526 </td>
   <td style="text-align:right;"> 0.3321438 </td>
   <td style="text-align:right;"> 2227.783 </td>
   <td style="text-align:right;"> 0.8245002 </td>
   <td style="text-align:right;"> 0.4097436 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G117.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.8061677 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 2.4114784 </td>
   <td style="text-align:right;"> 0.0159684 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G118.16.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.6565052 </td>
   <td style="text-align:right;"> 0.3342095 </td>
   <td style="text-align:right;"> 2227.346 </td>
   <td style="text-align:right;"> -1.9643525 </td>
   <td style="text-align:right;"> 0.0496134 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G118.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -1.1361847 </td>
   <td style="text-align:right;"> 0.3320513 </td>
   <td style="text-align:right;"> 2227.319 </td>
   <td style="text-align:right;"> -3.4217142 </td>
   <td style="text-align:right;"> 0.0006335 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G119.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.6128671 </td>
   <td style="text-align:right;"> 0.3320525 </td>
   <td style="text-align:right;"> 2227.322 </td>
   <td style="text-align:right;"> 1.8456931 </td>
   <td style="text-align:right;"> 0.0650694 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G119.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.8318088 </td>
   <td style="text-align:right;"> 0.3343044 </td>
   <td style="text-align:right;"> 2227.826 </td>
   <td style="text-align:right;"> 2.4881780 </td>
   <td style="text-align:right;"> 0.0129125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G120.16.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.8383957 </td>
   <td style="text-align:right;"> 0.3321833 </td>
   <td style="text-align:right;"> 2228.362 </td>
   <td style="text-align:right;"> 2.5238946 </td>
   <td style="text-align:right;"> 0.0116752 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G121.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0988526 </td>
   <td style="text-align:right;"> 0.3321438 </td>
   <td style="text-align:right;"> 2227.783 </td>
   <td style="text-align:right;"> 0.2976200 </td>
   <td style="text-align:right;"> 0.7660209 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G122.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.1361847 </td>
   <td style="text-align:right;"> 0.3320513 </td>
   <td style="text-align:right;"> 2227.319 </td>
   <td style="text-align:right;"> -0.4101314 </td>
   <td style="text-align:right;"> 0.6817490 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G122.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.6286115 </td>
   <td style="text-align:right;"> 0.3343597 </td>
   <td style="text-align:right;"> 2228.465 </td>
   <td style="text-align:right;"> 1.8800458 </td>
   <td style="text-align:right;"> 0.0602322 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G123.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.8888153 </td>
   <td style="text-align:right;"> 0.3320513 </td>
   <td style="text-align:right;"> 2227.319 </td>
   <td style="text-align:right;"> 2.6767410 </td>
   <td style="text-align:right;"> 0.0074887 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G124.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.3628671 </td>
   <td style="text-align:right;"> 0.3320525 </td>
   <td style="text-align:right;"> 2227.322 </td>
   <td style="text-align:right;"> 4.1043720 </td>
   <td style="text-align:right;"> 0.0000420 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G125.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0395706 </td>
   <td style="text-align:right;"> 0.3341014 </td>
   <td style="text-align:right;"> 2227.197 </td>
   <td style="text-align:right;"> -0.1184390 </td>
   <td style="text-align:right;"> 0.9057305 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G125.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.0373525 </td>
   <td style="text-align:right;"> 0.3341014 </td>
   <td style="text-align:right;"> 2227.197 </td>
   <td style="text-align:right;"> 3.1049029 </td>
   <td style="text-align:right;"> 0.0019274 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G126.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0916233 </td>
   <td style="text-align:right;"> 0.3321922 </td>
   <td style="text-align:right;"> 2228.385 </td>
   <td style="text-align:right;"> -0.2758141 </td>
   <td style="text-align:right;"> 0.7827164 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G126.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4633957 </td>
   <td style="text-align:right;"> 0.3321833 </td>
   <td style="text-align:right;"> 2228.362 </td>
   <td style="text-align:right;"> 1.3950000 </td>
   <td style="text-align:right;"> 0.1631549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G127.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.8835063 </td>
   <td style="text-align:right;"> 0.3341014 </td>
   <td style="text-align:right;"> 2227.197 </td>
   <td style="text-align:right;"> 2.6444255 </td>
   <td style="text-align:right;"> 0.0082404 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G127.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.1253853 </td>
   <td style="text-align:right;"> 0.3341838 </td>
   <td style="text-align:right;"> 2227.287 </td>
   <td style="text-align:right;"> 3.3675643 </td>
   <td style="text-align:right;"> 0.0007712 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G128.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.7633190 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 2.2839105 </td>
   <td style="text-align:right;"> 0.0224705 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G129.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.7138153 </td>
   <td style="text-align:right;"> 0.3320513 </td>
   <td style="text-align:right;"> 2227.319 </td>
   <td style="text-align:right;"> 2.1497140 </td>
   <td style="text-align:right;"> 0.0316852 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G130.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.0388153 </td>
   <td style="text-align:right;"> 0.3320513 </td>
   <td style="text-align:right;"> 2227.319 </td>
   <td style="text-align:right;"> 3.1284784 </td>
   <td style="text-align:right;"> 0.0017799 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G130.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.9083767 </td>
   <td style="text-align:right;"> 0.3321922 </td>
   <td style="text-align:right;"> 2228.385 </td>
   <td style="text-align:right;"> 2.7344913 </td>
   <td style="text-align:right;"> 0.0062971 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G131.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0500000 </td>
   <td style="text-align:right;"> 0.3318484 </td>
   <td style="text-align:right;"> 2227.040 </td>
   <td style="text-align:right;"> -0.1506712 </td>
   <td style="text-align:right;"> 0.8802487 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G131.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2583767 </td>
   <td style="text-align:right;"> 0.3321922 </td>
   <td style="text-align:right;"> 2228.385 </td>
   <td style="text-align:right;"> 0.7777928 </td>
   <td style="text-align:right;"> 0.4367737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G133.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.4511474 </td>
   <td style="text-align:right;"> 0.3321438 </td>
   <td style="text-align:right;"> 2227.783 </td>
   <td style="text-align:right;"> -1.3582892 </td>
   <td style="text-align:right;"> 0.1745095 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G135.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.6638469 </td>
   <td style="text-align:right;"> 0.3341838 </td>
   <td style="text-align:right;"> 2227.287 </td>
   <td style="text-align:right;"> 1.9864725 </td>
   <td style="text-align:right;"> 0.0471032 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G136.17.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.5083767 </td>
   <td style="text-align:right;"> 0.3321922 </td>
   <td style="text-align:right;"> 2228.385 </td>
   <td style="text-align:right;"> 1.5303692 </td>
   <td style="text-align:right;"> 0.1260673 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G137.17.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.5500000 </td>
   <td style="text-align:right;"> 0.3318484 </td>
   <td style="text-align:right;"> 2227.040 </td>
   <td style="text-align:right;"> 1.6573835 </td>
   <td style="text-align:right;"> 0.0975828 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G139.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2488526 </td>
   <td style="text-align:right;"> 0.3321438 </td>
   <td style="text-align:right;"> 2227.783 </td>
   <td style="text-align:right;"> 0.7492316 </td>
   <td style="text-align:right;"> 0.4537967 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G140.17.KRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4878671 </td>
   <td style="text-align:right;"> 0.3320525 </td>
   <td style="text-align:right;"> 2227.322 </td>
   <td style="text-align:right;"> 1.4692467 </td>
   <td style="text-align:right;"> 0.1419071 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G140.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4988909 </td>
   <td style="text-align:right;"> 0.3341014 </td>
   <td style="text-align:right;"> 2227.197 </td>
   <td style="text-align:right;"> 1.4932320 </td>
   <td style="text-align:right;"> 0.1355181 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G142.17.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.0710113 </td>
   <td style="text-align:right;"> 0.3342158 </td>
   <td style="text-align:right;"> 2227.364 </td>
   <td style="text-align:right;"> 3.2045501 </td>
   <td style="text-align:right;"> 0.0013719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G143.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.2162885 </td>
   <td style="text-align:right;"> 0.3342121 </td>
   <td style="text-align:right;"> 2227.358 </td>
   <td style="text-align:right;"> -0.6471592 </td>
   <td style="text-align:right;"> 0.5175956 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G145.17.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.4973409 </td>
   <td style="text-align:right;"> 0.3342095 </td>
   <td style="text-align:right;"> 2227.346 </td>
   <td style="text-align:right;"> 4.4802466 </td>
   <td style="text-align:right;"> 0.0000078 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G149.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0761474 </td>
   <td style="text-align:right;"> 0.3321438 </td>
   <td style="text-align:right;"> 2227.783 </td>
   <td style="text-align:right;"> -0.2292602 </td>
   <td style="text-align:right;"> 0.8186877 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hay.1416.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3628671 </td>
   <td style="text-align:right;"> 0.3320525 </td>
   <td style="text-align:right;"> 2227.322 </td>
   <td style="text-align:right;"> 1.0928002 </td>
   <td style="text-align:right;"> 0.2745997 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V002.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.5742640 </td>
   <td style="text-align:right;"> 0.3342095 </td>
   <td style="text-align:right;"> 2227.346 </td>
   <td style="text-align:right;"> 1.7182756 </td>
   <td style="text-align:right;"> 0.0858853 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V002.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 1.1383957 </td>
   <td style="text-align:right;"> 0.3321833 </td>
   <td style="text-align:right;"> 2228.362 </td>
   <td style="text-align:right;"> 3.4270104 </td>
   <td style="text-align:right;"> 0.0006213 </td>
  </tr>
</tbody>
</table>
##Internal Appearance by Cultivar x Site x Graft year

```
## Warning: 'CLD' will be deprecated. Its use is discouraged.
## See '? CLD' for an explanation. Use 'pwpp' or 'multcomp::cld' instead.
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> selections </th>
   <th style="text-align:right;"> emmean </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> lower.CL </th>
   <th style="text-align:right;"> upper.CL </th>
   <th style="text-align:left;"> .group </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> G131.16.TRC </td>
   <td style="text-align:right;"> 6.168251 </td>
   <td style="text-align:right;"> 0.2333814 </td>
   <td style="text-align:right;"> 164.4082 </td>
   <td style="text-align:right;"> 5.707440 </td>
   <td style="text-align:right;"> 6.629062 </td>
   <td style="text-align:left;"> A </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> G118.16.TRC </td>
   <td style="text-align:right;"> 6.333996 </td>
   <td style="text-align:right;"> 0.2333874 </td>
   <td style="text-align:right;"> 164.4222 </td>
   <td style="text-align:right;"> 5.873173 </td>
   <td style="text-align:right;"> 6.794818 </td>
   <td style="text-align:left;"> AB </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> G143.17.TRC </td>
   <td style="text-align:right;"> 6.405403 </td>
   <td style="text-align:right;"> 0.2349078 </td>
   <td style="text-align:right;"> 168.5834 </td>
   <td style="text-align:right;"> 5.941663 </td>
   <td style="text-align:right;"> 6.869142 </td>
   <td style="text-align:left;"> ABC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> G131.16.WGT </td>
   <td style="text-align:right;"> 6.431252 </td>
   <td style="text-align:right;"> 0.2332651 </td>
   <td style="text-align:right;"> 164.1681 </td>
   <td style="text-align:right;"> 5.970665 </td>
   <td style="text-align:right;"> 6.891838 </td>
   <td style="text-align:left;"> ABCD </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> G125.16.TRC </td>
   <td style="text-align:right;"> 6.490601 </td>
   <td style="text-align:right;"> 0.2348987 </td>
   <td style="text-align:right;"> 168.5607 </td>
   <td style="text-align:right;"> 6.026878 </td>
   <td style="text-align:right;"> 6.954323 </td>
   <td style="text-align:left;"> ABCDE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> G118.16.HGT </td>
   <td style="text-align:right;"> 6.513822 </td>
   <td style="text-align:right;"> 0.2349062 </td>
   <td style="text-align:right;"> 168.5799 </td>
   <td style="text-align:right;"> 6.050085 </td>
   <td style="text-align:right;"> 6.977559 </td>
   <td style="text-align:left;"> ABCDEF </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> G095.1617.HGT </td>
   <td style="text-align:right;"> 6.550840 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.087097 </td>
   <td style="text-align:right;"> 7.014584 </td>
   <td style="text-align:left;"> ABCDEFG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> G121.16.WGT </td>
   <td style="text-align:right;"> 6.573433 </td>
   <td style="text-align:right;"> 0.2333262 </td>
   <td style="text-align:right;"> 164.2835 </td>
   <td style="text-align:right;"> 6.112728 </td>
   <td style="text-align:right;"> 7.034138 </td>
   <td style="text-align:left;"> ABCDEFGH </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> G123.16.WGT </td>
   <td style="text-align:right;"> 6.583996 </td>
   <td style="text-align:right;"> 0.2333874 </td>
   <td style="text-align:right;"> 164.4222 </td>
   <td style="text-align:right;"> 6.123173 </td>
   <td style="text-align:right;"> 7.044818 </td>
   <td style="text-align:left;"> ABCDEFGHI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> G149.17.TRC </td>
   <td style="text-align:right;"> 6.598433 </td>
   <td style="text-align:right;"> 0.2333262 </td>
   <td style="text-align:right;"> 164.2835 </td>
   <td style="text-align:right;"> 6.137728 </td>
   <td style="text-align:right;"> 7.059138 </td>
   <td style="text-align:left;"> ABCDEFGHIJ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> G130.16.WGT </td>
   <td style="text-align:right;"> 6.606252 </td>
   <td style="text-align:right;"> 0.2332651 </td>
   <td style="text-align:right;"> 164.1681 </td>
   <td style="text-align:right;"> 6.145665 </td>
   <td style="text-align:right;"> 7.066838 </td>
   <td style="text-align:left;"> ABCDEFGHIJK </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> G119.16.WGT </td>
   <td style="text-align:right;"> 6.653190 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.189565 </td>
   <td style="text-align:right;"> 7.116815 </td>
   <td style="text-align:left;"> BCDEFGHIJKL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> G117.16.TRC </td>
   <td style="text-align:right;"> 6.723433 </td>
   <td style="text-align:right;"> 0.2333262 </td>
   <td style="text-align:right;"> 164.2835 </td>
   <td style="text-align:right;"> 6.262728 </td>
   <td style="text-align:right;"> 7.184138 </td>
   <td style="text-align:left;"> BCDEFGHIJKLM </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> G100.15.KRC </td>
   <td style="text-align:right;"> 6.730327 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.266584 </td>
   <td style="text-align:right;"> 7.194071 </td>
   <td style="text-align:left;"> BCDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> G129.16.WGT </td>
   <td style="text-align:right;"> 6.733996 </td>
   <td style="text-align:right;"> 0.2333874 </td>
   <td style="text-align:right;"> 164.4222 </td>
   <td style="text-align:right;"> 6.273173 </td>
   <td style="text-align:right;"> 7.194818 </td>
   <td style="text-align:left;"> BCDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> G106.15.TRC </td>
   <td style="text-align:right;"> 6.770232 </td>
   <td style="text-align:right;"> 0.2349062 </td>
   <td style="text-align:right;"> 168.5799 </td>
   <td style="text-align:right;"> 6.306495 </td>
   <td style="text-align:right;"> 7.233969 </td>
   <td style="text-align:left;"> BCDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> G117.16.WGT </td>
   <td style="text-align:right;"> 6.781395 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.317770 </td>
   <td style="text-align:right;"> 7.245020 </td>
   <td style="text-align:left;"> BCDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> G119.16.TRC </td>
   <td style="text-align:right;"> 6.804837 </td>
   <td style="text-align:right;"> 0.2333882 </td>
   <td style="text-align:right;"> 164.4241 </td>
   <td style="text-align:right;"> 6.344013 </td>
   <td style="text-align:right;"> 7.265661 </td>
   <td style="text-align:left;"> CDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> G122.16.WGT </td>
   <td style="text-align:right;"> 6.806252 </td>
   <td style="text-align:right;"> 0.2332651 </td>
   <td style="text-align:right;"> 164.1681 </td>
   <td style="text-align:right;"> 6.345665 </td>
   <td style="text-align:right;"> 7.266838 </td>
   <td style="text-align:left;"> CDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> Hay.141617.TRC </td>
   <td style="text-align:right;"> 6.818251 </td>
   <td style="text-align:right;"> 0.2333814 </td>
   <td style="text-align:right;"> 164.4082 </td>
   <td style="text-align:right;"> 6.357440 </td>
   <td style="text-align:right;"> 7.279063 </td>
   <td style="text-align:left;"> CDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> G133.17.TRC </td>
   <td style="text-align:right;"> 6.823433 </td>
   <td style="text-align:right;"> 0.2333262 </td>
   <td style="text-align:right;"> 164.2835 </td>
   <td style="text-align:right;"> 6.362728 </td>
   <td style="text-align:right;"> 7.284138 </td>
   <td style="text-align:left;"> CDEFGHIJKLMN </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> G113.16.WGT </td>
   <td style="text-align:right;"> 6.832677 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.369052 </td>
   <td style="text-align:right;"> 7.296302 </td>
   <td style="text-align:left;"> CDEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> G099.15.TRC </td>
   <td style="text-align:right;"> 6.848433 </td>
   <td style="text-align:right;"> 0.2333262 </td>
   <td style="text-align:right;"> 164.2835 </td>
   <td style="text-align:right;"> 6.387728 </td>
   <td style="text-align:right;"> 7.309138 </td>
   <td style="text-align:left;"> CDEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> G112.16.TRC </td>
   <td style="text-align:right;"> 6.858318 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.394693 </td>
   <td style="text-align:right;"> 7.321943 </td>
   <td style="text-align:left;"> CDEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> G106.15.WGT </td>
   <td style="text-align:right;"> 6.868251 </td>
   <td style="text-align:right;"> 0.2333814 </td>
   <td style="text-align:right;"> 164.4082 </td>
   <td style="text-align:right;"> 6.407440 </td>
   <td style="text-align:right;"> 7.329063 </td>
   <td style="text-align:left;"> DEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> G107.15.TRC </td>
   <td style="text-align:right;"> 6.872796 </td>
   <td style="text-align:right;"> 0.2349062 </td>
   <td style="text-align:right;"> 168.5799 </td>
   <td style="text-align:right;"> 6.409059 </td>
   <td style="text-align:right;"> 7.336533 </td>
   <td style="text-align:left;"> DEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> G127.16.TRC </td>
   <td style="text-align:right;"> 6.875216 </td>
   <td style="text-align:right;"> 0.2348987 </td>
   <td style="text-align:right;"> 168.5607 </td>
   <td style="text-align:right;"> 6.411494 </td>
   <td style="text-align:right;"> 7.338939 </td>
   <td style="text-align:left;"> DEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> G109.15.TRC </td>
   <td style="text-align:right;"> 6.884174 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.420430 </td>
   <td style="text-align:right;"> 7.347917 </td>
   <td style="text-align:left;"> DEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> G102.15.TRC </td>
   <td style="text-align:right;"> 6.889895 </td>
   <td style="text-align:right;"> 0.2348907 </td>
   <td style="text-align:right;"> 168.5402 </td>
   <td style="text-align:right;"> 6.426188 </td>
   <td style="text-align:right;"> 7.353602 </td>
   <td style="text-align:left;"> DEFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> G140.17.TRC </td>
   <td style="text-align:right;"> 6.900857 </td>
   <td style="text-align:right;"> 0.2348987 </td>
   <td style="text-align:right;"> 168.5607 </td>
   <td style="text-align:right;"> 6.437135 </td>
   <td style="text-align:right;"> 7.364580 </td>
   <td style="text-align:left;"> EFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> G100.15.TRC </td>
   <td style="text-align:right;"> 6.909815 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.446071 </td>
   <td style="text-align:right;"> 7.373558 </td>
   <td style="text-align:left;"> EFGHIJKLMNO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> G108.15.WGT </td>
   <td style="text-align:right;"> 6.941177 </td>
   <td style="text-align:right;"> 0.2348907 </td>
   <td style="text-align:right;"> 168.5402 </td>
   <td style="text-align:right;"> 6.477470 </td>
   <td style="text-align:right;"> 7.404884 </td>
   <td style="text-align:left;"> EFGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> G137.17.WGT </td>
   <td style="text-align:right;"> 6.943251 </td>
   <td style="text-align:right;"> 0.2333814 </td>
   <td style="text-align:right;"> 164.4082 </td>
   <td style="text-align:right;"> 6.482440 </td>
   <td style="text-align:right;"> 7.404063 </td>
   <td style="text-align:left;"> EFGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> V002.16.TRC </td>
   <td style="text-align:right;"> 6.949719 </td>
   <td style="text-align:right;"> 0.2349062 </td>
   <td style="text-align:right;"> 168.5799 </td>
   <td style="text-align:right;"> 6.485983 </td>
   <td style="text-align:right;"> 7.413456 </td>
   <td style="text-align:left;"> EFGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> G115.16.WGT </td>
   <td style="text-align:right;"> 6.960882 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.497257 </td>
   <td style="text-align:right;"> 7.424508 </td>
   <td style="text-align:left;"> FGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> G115.16.TRC </td>
   <td style="text-align:right;"> 6.960882 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.497257 </td>
   <td style="text-align:right;"> 7.424508 </td>
   <td style="text-align:left;"> FGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> G095.1617.WGT </td>
   <td style="text-align:right;"> 6.966818 </td>
   <td style="text-align:right;"> 0.2348907 </td>
   <td style="text-align:right;"> 168.5402 </td>
   <td style="text-align:right;"> 6.503111 </td>
   <td style="text-align:right;"> 7.430525 </td>
   <td style="text-align:left;"> FGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> G127.16.WGT </td>
   <td style="text-align:right;"> 6.966818 </td>
   <td style="text-align:right;"> 0.2348907 </td>
   <td style="text-align:right;"> 168.5402 </td>
   <td style="text-align:right;"> 6.503111 </td>
   <td style="text-align:right;"> 7.430525 </td>
   <td style="text-align:left;"> FGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> G135.17.TRC </td>
   <td style="text-align:right;"> 6.966818 </td>
   <td style="text-align:right;"> 0.2348907 </td>
   <td style="text-align:right;"> 168.5402 </td>
   <td style="text-align:right;"> 6.503111 </td>
   <td style="text-align:right;"> 7.430525 </td>
   <td style="text-align:left;"> FGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> G139.17.TRC </td>
   <td style="text-align:right;"> 6.973433 </td>
   <td style="text-align:right;"> 0.2333262 </td>
   <td style="text-align:right;"> 164.2835 </td>
   <td style="text-align:right;"> 6.512728 </td>
   <td style="text-align:right;"> 7.434138 </td>
   <td style="text-align:left;"> FGHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> G126.16.TRC </td>
   <td style="text-align:right;"> 6.981252 </td>
   <td style="text-align:right;"> 0.2332651 </td>
   <td style="text-align:right;"> 164.1681 </td>
   <td style="text-align:right;"> 6.520665 </td>
   <td style="text-align:right;"> 7.441838 </td>
   <td style="text-align:left;"> GHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> G122.16.TRC </td>
   <td style="text-align:right;"> 6.983996 </td>
   <td style="text-align:right;"> 0.2333874 </td>
   <td style="text-align:right;"> 164.4222 </td>
   <td style="text-align:right;"> 6.523173 </td>
   <td style="text-align:right;"> 7.444818 </td>
   <td style="text-align:left;"> GHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> G011.1314.HGT </td>
   <td style="text-align:right;"> 6.986738 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.522994 </td>
   <td style="text-align:right;"> 7.450481 </td>
   <td style="text-align:left;"> GHIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> G120.16.HGT </td>
   <td style="text-align:right;"> 7.016654 </td>
   <td style="text-align:right;"> 0.2332599 </td>
   <td style="text-align:right;"> 164.1550 </td>
   <td style="text-align:right;"> 6.556077 </td>
   <td style="text-align:right;"> 7.477230 </td>
   <td style="text-align:left;"> HIJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> G126.16.WGT </td>
   <td style="text-align:right;"> 7.041653 </td>
   <td style="text-align:right;"> 0.2332599 </td>
   <td style="text-align:right;"> 164.1550 </td>
   <td style="text-align:right;"> 6.581077 </td>
   <td style="text-align:right;"> 7.502230 </td>
   <td style="text-align:left;"> IJKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> G107.15.WGT </td>
   <td style="text-align:right;"> 7.043251 </td>
   <td style="text-align:right;"> 0.2333814 </td>
   <td style="text-align:right;"> 164.4082 </td>
   <td style="text-align:right;"> 6.582440 </td>
   <td style="text-align:right;"> 7.504062 </td>
   <td style="text-align:left;"> JKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> G125.16.WGT </td>
   <td style="text-align:right;"> 7.054703 </td>
   <td style="text-align:right;"> 0.2348987 </td>
   <td style="text-align:right;"> 168.5607 </td>
   <td style="text-align:right;"> 6.590981 </td>
   <td style="text-align:right;"> 7.518426 </td>
   <td style="text-align:left;"> JKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> G124.16.WGT </td>
   <td style="text-align:right;"> 7.054837 </td>
   <td style="text-align:right;"> 0.2333882 </td>
   <td style="text-align:right;"> 164.4241 </td>
   <td style="text-align:right;"> 6.594013 </td>
   <td style="text-align:right;"> 7.515661 </td>
   <td style="text-align:left;"> JKLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> G142.17.WGT </td>
   <td style="text-align:right;"> 7.063661 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.599917 </td>
   <td style="text-align:right;"> 7.527404 </td>
   <td style="text-align:left;"> KLMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> Hay.1416.WGT </td>
   <td style="text-align:right;"> 7.104837 </td>
   <td style="text-align:right;"> 0.2333882 </td>
   <td style="text-align:right;"> 164.4241 </td>
   <td style="text-align:right;"> 6.644013 </td>
   <td style="text-align:right;"> 7.565661 </td>
   <td style="text-align:left;"> LMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> G130.16.TRC </td>
   <td style="text-align:right;"> 7.108996 </td>
   <td style="text-align:right;"> 0.2333874 </td>
   <td style="text-align:right;"> 164.4222 </td>
   <td style="text-align:right;"> 6.648173 </td>
   <td style="text-align:right;"> 7.569818 </td>
   <td style="text-align:left;"> LMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> G128.16.WGT </td>
   <td style="text-align:right;"> 7.114943 </td>
   <td style="text-align:right;"> 0.2349097 </td>
   <td style="text-align:right;"> 168.5887 </td>
   <td style="text-align:right;"> 6.651199 </td>
   <td style="text-align:right;"> 7.578686 </td>
   <td style="text-align:left;"> LMNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> G140.17.KRC </td>
   <td style="text-align:right;"> 7.129837 </td>
   <td style="text-align:right;"> 0.2333882 </td>
   <td style="text-align:right;"> 164.4241 </td>
   <td style="text-align:right;"> 6.669013 </td>
   <td style="text-align:right;"> 7.590661 </td>
   <td style="text-align:left;"> MNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> V002.16.WGT </td>
   <td style="text-align:right;"> 7.166653 </td>
   <td style="text-align:right;"> 0.2332599 </td>
   <td style="text-align:right;"> 164.1550 </td>
   <td style="text-align:right;"> 6.706077 </td>
   <td style="text-align:right;"> 7.627230 </td>
   <td style="text-align:left;"> MNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> G136.17.HGT </td>
   <td style="text-align:right;"> 7.181252 </td>
   <td style="text-align:right;"> 0.2332651 </td>
   <td style="text-align:right;"> 164.1681 </td>
   <td style="text-align:right;"> 6.720665 </td>
   <td style="text-align:right;"> 7.641838 </td>
   <td style="text-align:left;"> MNOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> G102.15.WGT </td>
   <td style="text-align:right;"> 7.191651 </td>
   <td style="text-align:right;"> 0.2348484 </td>
   <td style="text-align:right;"> 168.4481 </td>
   <td style="text-align:right;"> 6.728026 </td>
   <td style="text-align:right;"> 7.655277 </td>
   <td style="text-align:left;"> NOP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> G109.15.WGT </td>
   <td style="text-align:right;"> 7.285473 </td>
   <td style="text-align:right;"> 0.2348987 </td>
   <td style="text-align:right;"> 168.5607 </td>
   <td style="text-align:right;"> 6.821750 </td>
   <td style="text-align:right;"> 7.749195 </td>
   <td style="text-align:left;"> OP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> G011.0814.GGT </td>
   <td style="text-align:right;"> 7.379837 </td>
   <td style="text-align:right;"> 0.2333882 </td>
   <td style="text-align:right;"> 164.4241 </td>
   <td style="text-align:right;"> 6.919013 </td>
   <td style="text-align:right;"> 7.840661 </td>
   <td style="text-align:left;"> PQ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> G145.17.WGT </td>
   <td style="text-align:right;"> 7.795873 </td>
   <td style="text-align:right;"> 0.2349062 </td>
   <td style="text-align:right;"> 168.5799 </td>
   <td style="text-align:right;"> 7.332136 </td>
   <td style="text-align:right;"> 8.259610 </td>
   <td style="text-align:left;"> Q </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> contrast </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> t.ratio </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> G011.0814.GGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.5615858 </td>
   <td style="text-align:right;"> 0.2337347 </td>
   <td style="text-align:right;"> 2228.143 </td>
   <td style="text-align:right;"> 2.4026635 </td>
   <td style="text-align:right;"> 0.0163575 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G011.1314.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1684864 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> 0.7161772 </td>
   <td style="text-align:right;"> 0.4739570 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G095.1617.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.2674110 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> -1.1366713 </td>
   <td style="text-align:right;"> 0.2557979 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G095.1617.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1485668 </td>
   <td style="text-align:right;"> 0.2352344 </td>
   <td style="text-align:right;"> 2228.125 </td>
   <td style="text-align:right;"> 0.6315691 </td>
   <td style="text-align:right;"> 0.5277333 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G099.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0301816 </td>
   <td style="text-align:right;"> 0.2338054 </td>
   <td style="text-align:right;"> 2228.386 </td>
   <td style="text-align:right;"> 0.1290886 </td>
   <td style="text-align:right;"> 0.8972992 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G100.15.KRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0879238 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> -0.3737337 </td>
   <td style="text-align:right;"> 0.7086380 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G100.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0915633 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> 0.3892039 </td>
   <td style="text-align:right;"> 0.6971625 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G102.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0716437 </td>
   <td style="text-align:right;"> 0.2352344 </td>
   <td style="text-align:right;"> 2228.125 </td>
   <td style="text-align:right;"> 0.3045630 </td>
   <td style="text-align:right;"> 0.7607274 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G102.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3734002 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> 1.5867300 </td>
   <td style="text-align:right;"> 0.1127157 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G106.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0480191 </td>
   <td style="text-align:right;"> 0.2352533 </td>
   <td style="text-align:right;"> 2228.153 </td>
   <td style="text-align:right;"> -0.2041168 </td>
   <td style="text-align:right;"> 0.8382809 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G106.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0500000 </td>
   <td style="text-align:right;"> 0.2335874 </td>
   <td style="text-align:right;"> 2228.010 </td>
   <td style="text-align:right;"> 0.2140527 </td>
   <td style="text-align:right;"> 0.8305256 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G107.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0545450 </td>
   <td style="text-align:right;"> 0.2352533 </td>
   <td style="text-align:right;"> 2228.153 </td>
   <td style="text-align:right;"> 0.2318563 </td>
   <td style="text-align:right;"> 0.8166709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G107.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2250000 </td>
   <td style="text-align:right;"> 0.2335874 </td>
   <td style="text-align:right;"> 2228.010 </td>
   <td style="text-align:right;"> 0.9632370 </td>
   <td style="text-align:right;"> 0.3355332 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G108.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1229258 </td>
   <td style="text-align:right;"> 0.2352344 </td>
   <td style="text-align:right;"> 2228.125 </td>
   <td style="text-align:right;"> 0.5225671 </td>
   <td style="text-align:right;"> 0.6013275 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G109.15.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0659223 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> 0.2802128 </td>
   <td style="text-align:right;"> 0.7793402 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G109.15.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.4672213 </td>
   <td style="text-align:right;"> 0.2351753 </td>
   <td style="text-align:right;"> 2228.083 </td>
   <td style="text-align:right;"> 1.9866938 </td>
   <td style="text-align:right;"> 0.0470786 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G112.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0400669 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> 0.1702605 </td>
   <td style="text-align:right;"> 0.8648207 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G113.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0144258 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> 0.0613013 </td>
   <td style="text-align:right;"> 0.9511247 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G115.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1426310 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> 0.6060973 </td>
   <td style="text-align:right;"> 0.5445119 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G115.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1426310 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> 0.6060973 </td>
   <td style="text-align:right;"> 0.5445119 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G117.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0948184 </td>
   <td style="text-align:right;"> 0.2338054 </td>
   <td style="text-align:right;"> 2228.386 </td>
   <td style="text-align:right;"> -0.4055440 </td>
   <td style="text-align:right;"> 0.6851166 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G117.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0368562 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> -0.1566171 </td>
   <td style="text-align:right;"> 0.8755608 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G118.16.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.3044294 </td>
   <td style="text-align:right;"> 0.2352533 </td>
   <td style="text-align:right;"> 2228.153 </td>
   <td style="text-align:right;"> -1.2940495 </td>
   <td style="text-align:right;"> 0.1957824 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G118.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.4842558 </td>
   <td style="text-align:right;"> 0.2337338 </td>
   <td style="text-align:right;"> 2228.141 </td>
   <td style="text-align:right;"> -2.0718259 </td>
   <td style="text-align:right;"> 0.0383965 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G119.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0134142 </td>
   <td style="text-align:right;"> 0.2337347 </td>
   <td style="text-align:right;"> 2228.143 </td>
   <td style="text-align:right;"> -0.0573906 </td>
   <td style="text-align:right;"> 0.9542392 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G119.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.1650613 </td>
   <td style="text-align:right;"> 0.2353269 </td>
   <td style="text-align:right;"> 2228.407 </td>
   <td style="text-align:right;"> -0.7014130 </td>
   <td style="text-align:right;"> 0.4831185 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G120.16.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1984022 </td>
   <td style="text-align:right;"> 0.2338424 </td>
   <td style="text-align:right;"> 2228.743 </td>
   <td style="text-align:right;"> 0.8484443 </td>
   <td style="text-align:right;"> 0.3962817 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G121.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.2448184 </td>
   <td style="text-align:right;"> 0.2338054 </td>
   <td style="text-align:right;"> 2228.386 </td>
   <td style="text-align:right;"> -1.0471031 </td>
   <td style="text-align:right;"> 0.2951656 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G122.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1657442 </td>
   <td style="text-align:right;"> 0.2337338 </td>
   <td style="text-align:right;"> 2228.141 </td>
   <td style="text-align:right;"> 0.7091154 </td>
   <td style="text-align:right;"> 0.4783270 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G122.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0119997 </td>
   <td style="text-align:right;"> 0.2338489 </td>
   <td style="text-align:right;"> 2228.754 </td>
   <td style="text-align:right;"> -0.0513138 </td>
   <td style="text-align:right;"> 0.9590801 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G123.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.2342558 </td>
   <td style="text-align:right;"> 0.2337338 </td>
   <td style="text-align:right;"> 2228.141 </td>
   <td style="text-align:right;"> -1.0022331 </td>
   <td style="text-align:right;"> 0.3163399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G124.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2365858 </td>
   <td style="text-align:right;"> 0.2337347 </td>
   <td style="text-align:right;"> 2228.143 </td>
   <td style="text-align:right;"> 1.0121981 </td>
   <td style="text-align:right;"> 0.3115532 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G125.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.3276505 </td>
   <td style="text-align:right;"> 0.2351753 </td>
   <td style="text-align:right;"> 2228.083 </td>
   <td style="text-align:right;"> -1.3932180 </td>
   <td style="text-align:right;"> 0.1636929 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G125.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2364521 </td>
   <td style="text-align:right;"> 0.2351753 </td>
   <td style="text-align:right;"> 2228.083 </td>
   <td style="text-align:right;"> 1.0054291 </td>
   <td style="text-align:right;"> 0.3147995 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G126.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1630003 </td>
   <td style="text-align:right;"> 0.2338489 </td>
   <td style="text-align:right;"> 2228.754 </td>
   <td style="text-align:right;"> 0.6970326 </td>
   <td style="text-align:right;"> 0.4858551 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G126.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2234022 </td>
   <td style="text-align:right;"> 0.2338424 </td>
   <td style="text-align:right;"> 2228.743 </td>
   <td style="text-align:right;"> 0.9553539 </td>
   <td style="text-align:right;"> 0.3395024 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G127.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0569649 </td>
   <td style="text-align:right;"> 0.2351753 </td>
   <td style="text-align:right;"> 2228.083 </td>
   <td style="text-align:right;"> 0.2422232 </td>
   <td style="text-align:right;"> 0.8086295 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G127.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1485668 </td>
   <td style="text-align:right;"> 0.2352344 </td>
   <td style="text-align:right;"> 2228.125 </td>
   <td style="text-align:right;"> 0.6315691 </td>
   <td style="text-align:right;"> 0.5277333 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G128.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2966915 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> 1.2611326 </td>
   <td style="text-align:right;"> 0.2073931 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G129.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.0842558 </td>
   <td style="text-align:right;"> 0.2337338 </td>
   <td style="text-align:right;"> 2228.141 </td>
   <td style="text-align:right;"> -0.3604774 </td>
   <td style="text-align:right;"> 0.7185243 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G130.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2907442 </td>
   <td style="text-align:right;"> 0.2337338 </td>
   <td style="text-align:right;"> 2228.141 </td>
   <td style="text-align:right;"> 1.2439118 </td>
   <td style="text-align:right;"> 0.2136629 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G130.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.2119997 </td>
   <td style="text-align:right;"> 0.2338489 </td>
   <td style="text-align:right;"> 2228.754 </td>
   <td style="text-align:right;"> -0.9065668 </td>
   <td style="text-align:right;"> 0.3647339 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G131.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.6500000 </td>
   <td style="text-align:right;"> 0.2335874 </td>
   <td style="text-align:right;"> 2228.010 </td>
   <td style="text-align:right;"> -2.7826845 </td>
   <td style="text-align:right;"> 0.0054366 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G131.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.3869997 </td>
   <td style="text-align:right;"> 0.2338489 </td>
   <td style="text-align:right;"> 2228.754 </td>
   <td style="text-align:right;"> -1.6549132 </td>
   <td style="text-align:right;"> 0.0980829 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G133.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0051816 </td>
   <td style="text-align:right;"> 0.2338054 </td>
   <td style="text-align:right;"> 2228.386 </td>
   <td style="text-align:right;"> 0.0221620 </td>
   <td style="text-align:right;"> 0.9823207 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G135.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1485668 </td>
   <td style="text-align:right;"> 0.2352344 </td>
   <td style="text-align:right;"> 2228.125 </td>
   <td style="text-align:right;"> 0.6315691 </td>
   <td style="text-align:right;"> 0.5277333 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G136.17.HGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3630003 </td>
   <td style="text-align:right;"> 0.2338489 </td>
   <td style="text-align:right;"> 2228.754 </td>
   <td style="text-align:right;"> 1.5522856 </td>
   <td style="text-align:right;"> 0.1207359 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G137.17.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1250000 </td>
   <td style="text-align:right;"> 0.2335874 </td>
   <td style="text-align:right;"> 2228.010 </td>
   <td style="text-align:right;"> 0.5351316 </td>
   <td style="text-align:right;"> 0.5926122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G139.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1551816 </td>
   <td style="text-align:right;"> 0.2338054 </td>
   <td style="text-align:right;"> 2228.386 </td>
   <td style="text-align:right;"> 0.6637211 </td>
   <td style="text-align:right;"> 0.5069375 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G140.17.KRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3115858 </td>
   <td style="text-align:right;"> 0.2337347 </td>
   <td style="text-align:right;"> 2228.143 </td>
   <td style="text-align:right;"> 1.3330747 </td>
   <td style="text-align:right;"> 0.1826436 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G140.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.0826059 </td>
   <td style="text-align:right;"> 0.2351753 </td>
   <td style="text-align:right;"> 2228.083 </td>
   <td style="text-align:right;"> 0.3512526 </td>
   <td style="text-align:right;"> 0.7254321 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G142.17.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2454095 </td>
   <td style="text-align:right;"> 0.2352580 </td>
   <td style="text-align:right;"> 2228.162 </td>
   <td style="text-align:right;"> 1.0431504 </td>
   <td style="text-align:right;"> 0.2969918 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G143.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.4128488 </td>
   <td style="text-align:right;"> 0.2352553 </td>
   <td style="text-align:right;"> 2228.159 </td>
   <td style="text-align:right;"> -1.7548964 </td>
   <td style="text-align:right;"> 0.0794145 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G145.17.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.9776219 </td>
   <td style="text-align:right;"> 0.2352533 </td>
   <td style="text-align:right;"> 2228.153 </td>
   <td style="text-align:right;"> 4.1556139 </td>
   <td style="text-align:right;"> 0.0000337 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G149.17.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> -0.2198184 </td>
   <td style="text-align:right;"> 0.2338054 </td>
   <td style="text-align:right;"> 2228.386 </td>
   <td style="text-align:right;"> -0.9401766 </td>
   <td style="text-align:right;"> 0.3472289 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hay.1416.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.2865858 </td>
   <td style="text-align:right;"> 0.2337347 </td>
   <td style="text-align:right;"> 2228.143 </td>
   <td style="text-align:right;"> 1.2261159 </td>
   <td style="text-align:right;"> 0.2202846 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V002.16.TRC - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.1314680 </td>
   <td style="text-align:right;"> 0.2352533 </td>
   <td style="text-align:right;"> 2228.153 </td>
   <td style="text-align:right;"> 0.5588361 </td>
   <td style="text-align:right;"> 0.5763297 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V002.16.WGT - Hay.141617.TRC </td>
   <td style="text-align:right;"> 0.3484022 </td>
   <td style="text-align:right;"> 0.2338424 </td>
   <td style="text-align:right;"> 2228.743 </td>
   <td style="text-align:right;"> 1.4899020 </td>
   <td style="text-align:right;"> 0.1363915 </td>
  </tr>
</tbody>
</table>
