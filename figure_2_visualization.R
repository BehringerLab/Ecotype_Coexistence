## Below is the code to generate all data figure components of Figure 2

## load required packages
library("tidyverse")
library("ggtext")
library("ggbeeswarm")
library("ggbreak")

## set theme for plots
willie <- 
  function (base_size = 18, base_family = "", base_line_size = base_size/22, 
            base_rect_size = base_size/22){
    theme_bw(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
      theme(text = element_text(size = base_size, family = base_family, face = 'bold', color = 'black'),
            panel.border = element_rect(color = "black", fill=NA, size=2),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black",
                                     linewidth = rel(1)), legend.key = element_blank(), 
            strip.background = element_rect(fill = "white", colour = "black", 
                                            linewidth = rel(2)), complete = TRUE)
  }


## To generate Panel 2B
dataset_3 <- read.csv("dataset_3.csv")

dataset_3 %>%
  dplyr::mutate(which_comp = factor(which_comp, levels = c("Auxotroph *∆metB*", "Overproducer *∆metJ*"))) %>%
  ggplot(aes(time, cfu, col=eco)) +
  stat_summary(
    fun = mean,
    geom='line',
    linewidth = 1.25,
    aes(colour=eco)) +
  geom_quasirandom(size = 1.75, width = 0.2, alpha = 0.4) +
  scale_color_manual(values = c("#d55e00", "#0072b2", "#ababab"))+
  geom_hline(yintercept = c(10^10, 10^8, 10^7, 10^9, 10^6), linetype = "dashed", alpha = 0.1) +
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.5) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  willie() +
  facet_grid(vars(facet),vars(which_comp)) +
  labs(x = 'Time (days)', y = 'CFU / mL') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        strip.text.y = element_text(face = "bold", size = 18),
        strip.text = ggtext::element_markdown(),
        axis.text = element_text(size = 18))

## To generate Panel 2C
dataset_4 <- read.csv("dataset_4.csv")

dataset_4 %>% 
  dplyr::filter(wrap %in% c("Selection Rate vs. wild-type")) %>%
  ggplot(aes(x=time, y=r, col=eco)) +
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  stat_summary(
    fun = mean,
    geom='line',
    linetype = "solid",
    size=1.25,
    aes(colour=eco)) +
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.5) +
  geom_quasirandom(size = 1.75, width = 0.2, alpha = 0.4) +
  willie() +
  scale_color_manual(labels = c("∆metB", "∆metJ"), values = c("#d55e00", "#0072b2")) +
  ylim(-1.5, 0.5) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  facet_wrap(~wrap, nrow = 2, ncol=1) +
  labs(x = 'Time (days)', y = 'Selection Rate (r)', colour = 'Strain') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        axis.text = element_text(size=18)
  )



## to generate panel 2E. Blank row and one errant evaporated row were filtered from this data set.
dataset_5 <- read.csv("dataset_5.csv")

dataset_5 %>%
  dplyr::mutate(background = factor(background, levels = c("wt", "mutant"))) %>%
  dplyr::mutate(media = factor(media, levels = c("Met (+)", "Met (-)"))) %>%
  ggplot(aes(background, k, col=strain, fill=strain))+
  stat_boxplot(geom = "boxplot", position = "dodge2", lwd=0.75) +
  willie() +
  ylim(0.2, 1.2) +
  scale_fill_manual(values = c("#d55e00", "#0072b2", "#ababab", "#ababab"))+
  scale_color_manual(values = c("black","black", "black", "black"))+
  labs(y="Carrying Capacity (K)") +
  facet_wrap(~media) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color="black", size=20, face="bold"),
        strip.text.x = element_text(face = "bold", size=18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(color="black", size=20, face="bold"),
        axis.text = element_text(face = "bold", size=18),
        legend.position = "none")

## Welch's t-test between wild-type and ∆metB carrying capacity
dataset_5_ttest <- dataset_5 %>% filter(strain %in% c("auxotroph ∆metB", "wild-type (supplemented)")) %>% dplyr::select(k,strain)
t.test(k~strain, data=dataset_5_ttest)

## generate panel 2F
dataset_6 <- read.csv("dataset_6.csv")

dataset_6 %>%
  dplyr::mutate(aa = factor(aa, levels = c("L-methionine", "L-aspartate"))) %>%
  dplyr::mutate(eco = factor(eco, levels = c("wtm", "wt", "co", "b", "j"))) %>%
  ggplot(aes(time, conc, col = eco)) +
  stat_summary(
    fun = mean,
    geom='line',
    linewidth = 1.75,
    aes(colour=eco)) +
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.65) +
  scale_fill_manual(values = c("black", "black", "black", "black")) +
  scale_color_manual(values = c("#4a4a4a", "#ababab", "#d55e00", "#0072b2")) +
  scale_x_continuous(breaks=seq(0, 18, 6)) +
  willie() +
  facet_wrap(~aa, scales= "free_y", nrow=2) +
  labs(x = 'Time (hours)', y = '[Extracellular] (µM)', colour = 'Strain') +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.position = "none", 
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 18),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank())

## Generate panel 2F inlay
dataset_6 %>%
  dplyr::filter(aa %in% c("L-aspartate")) %>%
  dplyr::filter(time %in% c(6)) %>%
  dplyr::mutate(eco = factor(eco, levels = c("wtm", "wt", "b", "j"))) %>%
  ggplot(aes(time, conc, col = eco, fill =eco)) +
  stat_summary(
    fun = mean,
    geom='bar',
    size = 0.75,
    position = position_dodge(width = 1),
    aes(colour=eco)) +
  stat_summary(
    fun.data=mean_se,
    position = position_dodge(width = 1),
    geom='errorbar',
    width=0.65,
    size=0.75) +
  scale_fill_manual(values = c("#4a4a4a", "#ababab", "#d55e00", "#0072b2"))+
  scale_color_manual(values = c("black","black","black","black", "black"))+
  scale_x_continuous(breaks=seq(0, 18, 6)) +
  facet_wrap(~aa, scales= "free_y", nrow=2) +
  labs(x = 'Asp @ 6 hrs', y = '[Extracellular] (µM)', colour = 'Strain') +
  theme_classic() +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.position = "none", 
        axis.title.x = element_text(face = "bold", size = 28),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = 22, face="bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size = 2),
        axis.line.x = element_line(size = 2),
        axis.line.y = element_line(size = 1.5)
  )

## Anova at hour 6 on aspartate externalization levels
dataset_6_anova <- dataset_6 %>% dplyr::filter(time %in% c(6)) %>% dplyr::filter(aa %in% c("L-aspartate"))
dataset_6_model <- aov(conc ~ eco, dataset_6_anova)
TukeyHSD(dataset_6_model)


## To generate panel 2I
dataset_7 <- read.csv("dataset_7.csv")

ancestor_labels <- c("Met (-) Mutant Co-Culture")
names(ancestor_labels) <- c("ancestor")

dataset_7 %>%
  dplyr::filter(!(media %in% c("LB"))) %>%
  ggplot(aes(time, cfu, col=strain)) +
  stat_summary(
    fun = mean,
    geom='line',
    size = 1.25,
    aes(colour=strain)) +
  geom_quasirandom(size = 1.75, width = 0.2, alpha = 0.4) +
  scale_color_manual(labels = c("Auxotroph", "Overproducer"), values = c("#d55e00", "#0072b2"))+
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.5) +
  geom_hline(yintercept = c(10^8, 10^7, 10^9, 10^6), linetype = "dashed", alpha = 0.1) +
  scale_y_log10() +
  scale_x_continuous(breaks=seq(0,10,2)) +
  facet_wrap(~evo_group, labeller = labeller(evo_group=ancestor_labels)) +
  willie() +
  labs(x = 'Time (days)', y = 'CFU / mL') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18))

## Welch's t-test for abundances at day 10 in unsupplemented co-culture between both mutants
dataset_7_ttest <- dataset_7 %>% filter(time %in% c(10)) %>% dplyr::select(cfu,strain)
t.test(cfu~strain, data=dataset_7_ttest)

## to generate panel 2I inlay
dataset_8 <- read.csv("dataset_8.csv")

dataset_8 %>%
  ggplot(aes(time_hours, abundance, col=ecotype)) +
  stat_summary(
    fun = mean,
    geom='line',
    size = 1.5,
    aes(colour=ecotype)) +
  geom_quasirandom(size = 1.75, width = 0.2, alpha = 0.4) +
  scale_color_manual(values = c("#d55e00", "#0072b2"))+
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.5) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(0,6,12,18,24)) +
  theme_classic() +
  labs(x="Time (hours)", y= "CFU / mL") +
  theme(legend.title = element_text(face = "bold", size = 18),
        legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 28),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_text(size = 22, face="bold"),
        axis.ticks = element_line(size = 2),
        axis.line.x = element_line(size = 2),
        axis.line.y = element_line(size = 1.5))

## To generate panel 2J
dataset_9 <- read.csv("dataset_9.csv")

percent_labels <- c("Met (+)", "Met (-)")
names(percent_labels) <- c("met", "no_met")

dataset_9 %>%
  ggplot(aes(time_gap, percent*100, col=eco_by_vessel, fill = strain)) +
  willie() +
  geom_hline(yintercept = 0, linetype="solid") +
  stat_summary(
    fun = mean,
    geom='bar',
    position = position_dodge(width = 0.9),
    size = 0.5,
    aes(colour=strain)) +
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    position = position_dodge(width = 0.9),
    width=0.5,
    colour="black") +
  ylim(-100, 8000) +
  scale_y_break(c(400, 900), scales = c(0.4, 1)) +
  scale_fill_manual(labels = c("Auxotroph ∆metB", "Overproducer ∆metJ"), values = c("#d55e00", "#0072b2", "#ffbf00", "#009e73"))+
  scale_color_manual(labels = c("Auxotroph ∆metB", "Overproducer ∆metJ"), values = c("black","black","black","black"))+
  facet_grid(vars(facet), vars(condition), scales = "free", labeller = labeller(condition=percent_labels)) +
  labs(x = '          Time Period (days)', y = '% Change in Density \n in Co-Culture', colour = 'Ecotype') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 22),
        axis.title.y = element_text(face = "bold", size = 22),
        strip.text.x = element_text(face = "bold", size = 20),
        strip.text.y = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75, size = 18),
        axis.text.y = element_text(size = 18))

## To generate panel 2K
dataset_10 <- read.csv("dataset_10.csv")

met_labels <- c("Met (-)", "Met (+)", "Flask")
names(met_labels) <- c("tube", "tube_met", "flask")

dataset_10 %>%
  dplyr::mutate(evo_group = factor(evo_group, levels = c("tube_met", "tube"))) %>%
  ggplot(aes(time, cfu_diff_day, col=strain)) +
  geom_hline(yintercept=0, linetype="dashed")+
  stat_summary(
    fun = mean,
    geom='line',
    size = 1.5,
    aes(colour=strain)) +
  scale_color_manual(labels = c("∆metB", "∆metJ"), values = c("#d55e00", "#0072b2"))+
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.5) +
  scale_y_continuous(breaks=c(-5e8, 0, 5e8, 1e9, 1.5e9)) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  facet_wrap(~evo_group, labeller = labeller(evo_group=met_labels)) +
  willie() +
  labs(x = 'Time (days)', y = '∆CFU / mL / day', colour = 'Ecotype') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
  )

