# Adapted from Carpenter et al 2022
# Light and photoacclimatization drive distinct differences between shallow and mesophotic coral communities



fire.data <- read_csv("/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Cladocora/Data/FIRe Sep 2023/Cladocora_Leonit _and _Naharia.csv")

#FIRe Data

library(tidyverse)
install.packages("lubridate")
library(broom)
library(lubridate)
library(cowplot)
library(tidyverse)
library(ggtext)

fire.data$depth <- as_factor(fire.data$depth)


is.na(fire.data$depth) #Line 421 - line 425
fire.data <- na.omit(fire.data)

fvfm <- ggplot(data = fire.data,aes(x= depth, y=Fv/Fm, fill=Species))+
  geom_boxplot() +
  theme_set(theme_cowplot()) +
  labs( y="Fv’/Fm’") +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  #scale_fill_manual(values=c("#4393C3","#F4A582")) +
  scale_x_discrete(limits = rev) +
  coord_flip()

fvfm

sigma <- ggplot(data = fire.data,aes(x= depth, y=Sigma, fill=Species))+
  geom_boxplot() +
  theme_set(theme_cowplot()) +
  labs( y= "σPSII’(A2)") +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  #  scale_fill_manual(values=c("#4393C3","#F4A582")) +
  scale_x_discrete(limits = rev) +
  coord_flip()
sigma

pmax <- ggplot(data = fire.data,aes(x= depth, y=Pmax.e.s, fill=Species))+
  geom_boxplot() +
  theme_set(theme_cowplot()) +
  labs( y="Pmax (electron s-1 PSII-1)") +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  #  scale_fill_manual(values=c("#4393C3","#F4A582")) +
  scale_x_discrete(limits = rev) +
  coord_flip()
pmax

p <- ggplot(data = fire.data,aes(x= depth, y=p, fill=Species))+
  geom_boxplot() +
  theme_set(theme_cowplot()) +
  labs( y="connectivity (p)") +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  #scale_fill_manual(values=c("#4393C3","#F4A582")) +
  scale_x_discrete(limits = rev) +
  coord_flip()
p

#p_legend <- ggplot(data = fire.data,aes(x= depth, y=p, fill=Species))+
  #geom_boxplot() +
  #theme_set(theme_cowplot()) +
 # labs(title="p") +
  #  scale_fill_manual(values=c("#4393C3","#F4A582")) +
  #coord_flip()
#p_legend

nolegend <- plot_grid(fvfm,sigma,pmax, p, labels = c('a','b','c','d'))

#legend <- get_legend(p_legend)
fire_plot <- plot_grid(nolegend,legend, rel_widths = c(1,0.25))
fire_plot



#FIRe STATS
```{r}
library("FSA")
library("rcompanion")

#Kruskal-Wallis Test- data is not normally distributed
fire.data$Depth_group <- as_factor(fire.data$depth)
levels(fire.data$Depth_group)
fire.data <- subset(fire.data, Species == "Cladocora")


#FvFm
#First, look at differences between species
KW_fvfm_s <- kruskal.test(Fv/Fm ~ Species, data = fire.data) #null = there is no difference in value
KW_fvfm_s



# sigma 

KW_sigma <- kruskal.test(Sigma ~ depth, data = fire.data)
KW_sigma


#Pmax
#Then, look at differences between depts for each species
KW_pmax <- kruskal.test(Pmax.e.s ~ depth, data = fire.data)
KW_pmax

```
#LI-CORE
```{r}

library(ggplot2)

licore_data <- read.csv('LICOR_06242021.csv')
licore_data$Depth <- as.factor(licore_data$Depth)
licore <-ggplot(licore_data, aes(Depth, INPUT1, fill=Reading)) +
  geom_boxplot()
licore

licore_percent_all <- licore_data %>%
  mutate(percent_of_max = INPUT1/311.12200) #Solar noon surface = 311.12200

licore_percent_plot <- ggplot(licore_percent_all, aes(x=Depth,y=percent_of_max)) +
  geom_boxplot()
licore_percent_plot

licore_means <- licore_data %>%            #  THIS IS THE DATA SET USED TO MAKE PAR DATA IN MS
  group_by(Depth, Reading) %>%
  summarise(PAR_means = mean(INPUT1),
            std_dev = sd(INPUT1)) %>%
  ungroup()

licore_percent <- read.csv('licor_percents.csv')

summary(licore_means)

licore_aov <- aov (INPUT1 ~ Reading * Depth, data=licore_data)
anova(licore_aov)
```

