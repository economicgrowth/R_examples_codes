##### ------------------------------- ######
##### ------------------------------- ######
#       Economic Growth & Development  
#       Created by: Vileidy Gonzalez y Juan Carlos Muñoz
#       Creation Date: Marzo 5
#       Description:
#       Descriptive Analysis
##### ------------------------------- ######
##### ------------------------------- ######
#####
##### Libraries
library(tidyverse)
library(WDI)


#main_path <- "/Users/juan-carlosm/OneDrive - Universidad EAFIT/Colombia_Agri-food_Migration/"

main_path <- "/Users/juan-carlosm/OneDrive - Universidad EAFIT/Colombia_Agri-food_Migration/"



graph_path <- paste0(main_path,"15_Agrifood_Stats/figures/")

##################
##### 01 - Economy
##################

### Gent Data
dat <- WDI(indicator='NY.GDP.PCAP.KD.ZG', country=c('CO','LCN'), start=1960, end=2019) %>% select(year,country,'NY.GDP.PCAP.KD.ZG')
  
colnames(dat) <- c("year","country","var")
lab <- attributes(dat$var)$label
  
print(dat %>% group_by(country) %>% dplyr::summarise(var_m=mean(var,na.rm=TRUE),var_sd=sd(var,na.rm=FALSE)))
  
### Make Graph
ggplot(dat, aes(year, var, color=country)) +
    geom_line() + xlab('') + ylab(lab)+labs(color="",size=12)+  # line color
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust=0.5, size = 12),axis.text.y = element_text(angle = 90, vjust=0.5, size = 12),panel.grid.minor = element_blank(),legend.text=element_text(size=12))+ 
    scale_x_continuous(breaks = seq(1960,2020, by = 5))  # turn off minor grid
  

